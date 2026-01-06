// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <string>
#include <utility>
#include <cmath>
#include <sstream>
#include <algorithm>

using namespace Rcpp;

// Global debug flag
bool useDebug = false;

// Debug functions - conditionally defined
#ifndef DebugPrintText
void DebugPrintText(std::string text) {
    if (useDebug) {
        Rcout << text << "\n";
    }
}
#else
#undef DebugPrintText
void DebugPrintText(std::string text) {
    if (useDebug) {
        Rcout << text << "\n";
    }
}
#endif

#ifndef DebugPrintValue
void DebugPrintValue(std::string label, double value) {
    if (useDebug) {
        Rcout << (label + ": ") << value << "\n";
    }
}
#else
#undef DebugPrintValue
void DebugPrintValue(std::string label, double value) {
    if (useDebug) {
        Rcout << (label + ": ") << value << "\n";
    }
}
#endif

// ============================================================================
// Helper Functions for Performance and Clean Code
// ============================================================================

// Compact key for (fromState, toState): from * nStates + to
inline int edge_key(int from, int to, int nStates) noexcept {
  return from * nStates + to;
}

// Build "from..to" rownames without R callbacks
static CharacterVector make_cycle_rownames(int from, int to) {
  int len = to - from + 1;
  CharacterVector out(len);
  for (int i = 0; i < len; ++i) {
    out[i] = std::to_string(from + i);
  }
  return out;
}

// Optimized ValEntry with precomputed constant flag and value
struct ValEntry {
  int valueIndex;
  NumericVector series;   // length 1 (const) or nCycles (time-varying); shallow SEXP ref
  bool isConst;
  double constVal;
};

// State name parsing result
struct StateNameParts {
  std::string collapsed_name;
  int state_time;
};

// Parse state name efficiently without regex
StateNameParts parseStateName(const std::string& expanded_name) {
  size_t dot_pos = expanded_name.rfind('.');
  if (dot_pos != std::string::npos && dot_pos < expanded_name.length() - 1) {
    // Check if what follows the dot is a number
    std::string time_str = expanded_name.substr(dot_pos + 1);
    bool is_number = true;
    for (char c : time_str) {
      if (!std::isdigit(c)) {
        is_number = false;
        break;
      }
    }
    if (is_number && !time_str.empty()) {
      return {
        expanded_name.substr(0, dot_pos),
        std::stoi(time_str)
      };
    }
  }
  return {expanded_name, 1};
}

// Format ranges efficiently (e.g., [1,2,3,5,7,8,9] -> "1-3, 5, 7-9")
std::string formatRanges(const std::vector<int>& numbers) {
  if (numbers.empty()) return "N/A";

  std::vector<int> sorted = numbers;
  std::sort(sorted.begin(), sorted.end());
  sorted.erase(std::unique(sorted.begin(), sorted.end()), sorted.end());

  if (sorted.empty()) return "N/A";

  std::stringstream ss;
  int range_start = sorted[0];
  int range_end = sorted[0];

  for (size_t i = 1; i < sorted.size(); ++i) {
    if (sorted[i] == range_end + 1) {
      range_end = sorted[i];
    } else {
      // Output the previous range
      if (range_start == range_end) {
        ss << range_start;
      } else {
        ss << range_start << "-" << range_end;
      }
      ss << ", ";
      range_start = sorted[i];
      range_end = sorted[i];
    }
  }

  // Output the last range
  if (range_start == range_end) {
    ss << range_start;
  } else {
    ss << range_start << "-" << range_end;
  }

  return ss.str();
}


// ============================================================================
// Optimized cppMarkovTransitionsAndTrace with Performance Improvements
// ============================================================================

// [[Rcpp::export]]
List cppMarkovTransitionsAndTrace(
    NumericMatrix transitions,
    DataFrame valuesTransitional,
    DataFrame valuesResidency,
    DataFrame modelStartValues,
    NumericVector initialProbs,
    CharacterVector stateNames,
    CharacterVector valueNames,
    int nCycles,
    double complementConstant,
    std::string halfCycleMethod = "start"
) {
  DebugPrintText("=== cppMarkovTransitionsAndTrace START ===");
  const int nValues = valueNames.size();
  const int nStates = stateNames.size();
  DebugPrintValue("nValues", nValues);
  DebugPrintValue("nStates", nStates);
  DebugPrintValue("nCycles", nCycles);

  // Defensive checks
  if (nStates == 0) stop("cppMarkovTransitionsAndTrace: No states provided (nStates = 0)");
  if (initialProbs.size() != nStates) {
    stop("cppMarkovTransitionsAndTrace: initialProbs length does not match number of states");
  }
  if (nCycles <= 0) stop("cppMarkovTransitionsAndTrace: nCycles must be positive");
  if (nValues < 0) stop("cppMarkovTransitionsAndTrace: nValues cannot be negative");

  // ---------- Precompute name->index maps ----------
  std::unordered_map<std::string, int> stateIndex;
  stateIndex.reserve(static_cast<size_t>(nStates) * 2);
  for (int i = 0; i < nStates; ++i) {
    stateIndex.emplace(as<std::string>(stateNames[i]), i);
  }

  std::unordered_map<std::string, int> valueIndex;
  valueIndex.reserve(static_cast<size_t>(nValues) * 2);
  for (int i = 0; i < nValues; ++i) {
    valueIndex.emplace(as<std::string>(valueNames[i]), i);
  }

  // ---------- Build compact dictionaries ----------
  std::unordered_map<int, std::vector<ValEntry>> transVals;
  std::unordered_map<int, std::vector<ValEntry>> residVals;

  // Transitional dictionary
  if (valuesTransitional.nrow() > 0) {
    CharacterVector t_from = valuesTransitional["state"];
    CharacterVector t_to   = valuesTransitional["destination"];
    List t_vals            = valuesTransitional["values_list"];
    const int n = valuesTransitional.nrow();
    transVals.reserve(static_cast<size_t>(n) * 2);

    for (int i = 0; i < n; ++i) {
      auto itF = stateIndex.find(as<std::string>(t_from[i]));
      auto itT = stateIndex.find(as<std::string>(t_to[i]));
      if (itF == stateIndex.end() || itT == stateIndex.end()) {
        if (itF == stateIndex.end()) {
          Rcpp::warning("Transitional value state '" + as<std::string>(t_from[i]) +
                        "' not found in state index. Values will be skipped.");
        }
        if (itT == stateIndex.end()) {
          Rcpp::warning("Transitional value destination '" + as<std::string>(t_to[i]) +
                        "' not found in state index. Values will be skipped.");
        }
        continue;
      }
      const int from = itF->second;
      const int to = itT->second;

      List lst = t_vals[i];
      CharacterVector nm = lst.names();
      std::vector<ValEntry> entries;
      entries.reserve(lst.size());

      for (int j = 0; j < lst.size(); ++j) {
        auto itV = valueIndex.find(as<std::string>(nm[j]));
        if (itV == valueIndex.end()) continue;
        NumericVector s = lst[j];
        const bool isConst = (s.size() == 1);
        const double cval = isConst ? s[0] : 0.0;
        entries.push_back(ValEntry{itV->second, s, isConst, cval});
      }
      transVals.emplace(edge_key(from, to, nStates), std::move(entries));
    }
  }

  // Residency dictionary
  if (valuesResidency.nrow() > 0) {
    CharacterVector r_state = valuesResidency["state"];
    List r_vals             = valuesResidency["values_list"];
    const int n = valuesResidency.nrow();
    residVals.reserve(static_cast<size_t>(n) * 2);

    for (int i = 0; i < n; ++i) {
      auto itS = stateIndex.find(as<std::string>(r_state[i]));
      if (itS == stateIndex.end()) {
        Rcpp::warning("Residency value state '" + as<std::string>(r_state[i]) +
                      "' not found in state index. Values will be skipped.");
        continue;
      }
      const int s = itS->second;

      List lst = r_vals[i];
      CharacterVector nm = lst.names();
      std::vector<ValEntry> entries;
      entries.reserve(lst.size());

      for (int j = 0; j < lst.size(); ++j) {
        auto itV = valueIndex.find(as<std::string>(nm[j]));
        if (itV == valueIndex.end()) continue;
        NumericVector q = lst[j];
        const bool isConst = (q.size() == 1);
        const double cval = isConst ? q[0] : 0.0;
        entries.push_back(ValEntry{itV->second, q, isConst, cval});
      }
      residVals.emplace(s, std::move(entries));
    }
  }

  // Model start values: last row wins
  std::vector<double> modelStartVec(static_cast<size_t>(nValues), 0.0);
  if (modelStartValues.nrow() > 0) {
    List ms_vals_col = modelStartValues["values_list"];
    List last = ms_vals_col[modelStartValues.nrow() - 1];
    CharacterVector nm = last.names();
    for (int j = 0; j < last.size(); ++j) {
      auto itV = valueIndex.find(as<std::string>(nm[j]));
      if (itV == valueIndex.end()) continue;
      NumericVector v = last[j];
      if (v.size() > 0) modelStartVec[static_cast<size_t>(itV->second)] = v[0];
    }
  }

  // ---------- Allocate outputs ----------
  const int transRows = transitions.nrow();
  NumericMatrix trace(nCycles + 1, nStates);
  colnames(trace) = stateNames;
  rownames(trace) = make_cycle_rownames(0, nCycles);

  NumericMatrix transitionalValueResults(nCycles, nValues);
  colnames(transitionalValueResults) = valueNames;
  rownames(transitionalValueResults) = make_cycle_rownames(1, nCycles);

  NumericMatrix uncondTransProbs(transRows, 4);
  colnames(uncondTransProbs) = CharacterVector::create("cycle", "from", "to", "value");

  LogicalMatrix transitionErrors(transRows, 5);
  colnames(transitionErrors) = CharacterVector::create(
    "complement", "probLessThanZero", "probGreaterThanOne", "sumNotEqualOne", "NaOrNaN"
  );

  // Initial trace row
  for (int i = 0; i < nStates; ++i) trace(0, i) = initialProbs[i];

  // ---------- FAST PATH PRECOMPUTATIONS ----------
  const int COL_CYCLE = 0, COL_FROM = 1, COL_TO = 2, COL_PROB = 3;
  const int totalTransRows = transRows;

  // Column views for faster access
  NumericMatrix::Column probCol = transitions(_, COL_PROB);

  // Typed copies for int-like columns (computed once)
  std::vector<int> fromCol(totalTransRows);
  std::vector<int> toCol(totalTransRows);
  std::vector<int> cycleCol(totalTransRows);
  for (int r = 0; r < totalTransRows; ++r) {
    fromCol[r]  = static_cast<int>(transitions(r, COL_FROM)) - 1; // to 0-based
    toCol[r]    = static_cast<int>(transitions(r, COL_TO))   - 1; // to 0-based
    cycleCol[r] = static_cast<int>(transitions(r, COL_CYCLE));
  }

  // Cache pointers to transitional value buckets by row
  std::vector<const std::vector<ValEntry>*> tvPtr(totalTransRows, nullptr);
  if (!transVals.empty()) {
    for (int r = 0; r < totalTransRows; ++r) {
      const int fk = edge_key(fromCol[r], toCol[r], nStates);
      auto it = transVals.find(fk);
      if (it != transVals.end()) tvPtr[r] = &(it->second);
    }
  }

  // Cache residency vector pointers by state
  std::vector<const std::vector<ValEntry>*> residPtr(nStates, nullptr);
  if (!residVals.empty()) {
    for (int s = 0; s < nStates; ++s) {
      auto it = residVals.find(s);
      if (it != residVals.end()) residPtr[s] = &(it->second);
    }
  }

  // ---------- Main transition loop ----------
  constexpr double FPOINT_TOL = 1e-9;
  int currentRow = 0;

  for (int cycle = 1; cycle <= nCycles; ++cycle) {
    NumericMatrix::Row prevTrace = trace.row(cycle - 1);
    NumericMatrix::Row currTrace = trace.row(cycle);

    for (int fromState = 0; fromState < nStates; ++fromState) {
      bool doneWithCurrentState = false;
      int complementsFoundInState = 0;
      int complementRowIndex = -1;
      double cumulativeProbability = 0.0;

      do {
        const int toState  = toCol[currentRow];
        const double value = probCol[currentRow];

        if (value == complementConstant) {
          ++complementsFoundInState;
          complementRowIndex = currentRow;
          transitionErrors(currentRow, 0) = (complementsFoundInState > 1);
        } else {
          cumulativeProbability += value;
          const double uncond = prevTrace[fromState] * value;

          currTrace[toState] += uncond;

          // Save unconditional probability row
          uncondTransProbs(currentRow, 0) = cycle;      // 1-based cycle
          uncondTransProbs(currentRow, 1) = fromState;  // 0-based
          uncondTransProbs(currentRow, 2) = toState;    // 0-based
          uncondTransProbs(currentRow, 3) = uncond;

          // Transitional values via cached pointer
          if (const std::vector<ValEntry>* tv = tvPtr[currentRow]) {
            for (const ValEntry& ve : *tv) {
              const double vv = ve.isConst ? ve.constVal : ve.series[cycle - 1];
              transitionalValueResults(cycle - 1, ve.valueIndex) += uncond * vv;
            }
          }

          // Error flags
          transitionErrors(currentRow, 0) = (complementsFoundInState > 1);
          transitionErrors(currentRow, 1) = (value < -FPOINT_TOL);
          transitionErrors(currentRow, 2) = (value > 1.0 + FPOINT_TOL);
          transitionErrors(currentRow, 4) = R_IsNA(value) || std::isnan(value);
        }

        // Advance & detect group boundary
        ++currentRow;
        if (currentRow >= totalTransRows) {
          doneWithCurrentState = true;
        } else {
          const int nextFrom  = fromCol[currentRow];
          const int nextCycle = cycleCol[currentRow];
          doneWithCurrentState = (nextFrom != fromState) || (nextCycle != cycle);
        }
      } while (!doneWithCurrentState);

      // Handle complement if present
      if (complementsFoundInState > 0) {
        const int toState = toCol[complementRowIndex];
        const double complementValue = 1.0 - cumulativeProbability;

        // Write back into transitions
        probCol[complementRowIndex] = complementValue;

        const double uncond = prevTrace[fromState] * complementValue;
        currTrace[toState] += uncond;

        // Record unconditional prob for complement row
        uncondTransProbs(complementRowIndex, 0) = cycle;
        uncondTransProbs(complementRowIndex, 1) = fromState;
        uncondTransProbs(complementRowIndex, 2) = toState;
        uncondTransProbs(complementRowIndex, 3) = uncond;

        // Transitional values for complement using cached pointer
        if (const std::vector<ValEntry>* tv = tvPtr[complementRowIndex]) {
          for (const ValEntry& ve : *tv) {
            const double vv = ve.isConst ? ve.constVal : ve.series[cycle - 1];
            transitionalValueResults(cycle - 1, ve.valueIndex) += uncond * vv;
          }
        }

        transitionErrors(complementRowIndex, 1) = (complementValue < -FPOINT_TOL);
        transitionErrors(complementRowIndex, 2) = (complementValue > 1.0 + FPOINT_TOL);
        transitionErrors(complementRowIndex, 4) = R_IsNA(complementValue) || std::isnan(complementValue);
      } else {
        // No complement: flag sum != 1 on the last processed row
        transitionErrors(currentRow - 1, 3) = (std::fabs(cumulativeProbability - 1.0) > FPOINT_TOL);
      }
    }
  }

  // ---------- Residency values ----------
  NumericMatrix residencyResults(nCycles, nValues);
  colnames(residencyResults) = valueNames;
  rownames(residencyResults) = make_cycle_rownames(1, nCycles);

  enum { HCM_START, HCM_END, HCM_LIFE_TABLE } hcm = HCM_START;
  if (halfCycleMethod == "end") hcm = HCM_END;
  else if (halfCycleMethod == "life-table") hcm = HCM_LIFE_TABLE;

  for (int cycle = 1; cycle <= nCycles; ++cycle) {
    NumericMatrix::Row prevTrace = trace.row(cycle - 1);
    NumericMatrix::Row currTrace = trace.row(cycle);

    for (int s = 0; s < nStates; ++s) {
      double stateProb;
      if (hcm == HCM_END) {
        stateProb = currTrace[s];
      } else if (hcm == HCM_LIFE_TABLE) {
        if (cycle == 1) stateProb = 0.5 * (trace(0, s) + trace(1, s));
        else if (cycle == nCycles) stateProb = currTrace[s];
        else stateProb = 0.5 * (prevTrace[s] + currTrace[s]);
      } else { // start
        stateProb = prevTrace[s];
      }

      if (const std::vector<ValEntry>* rv = residPtr[s]) {
        for (const ValEntry& ve : *rv) {
          const double vv = ve.isConst ? ve.constVal : ve.series[cycle - 1];
          residencyResults(cycle - 1, ve.valueIndex) += stateProb * vv;
        }
      }
    }
  }

  // ---------- Model start values (first cycle only) ----------
  NumericMatrix modelStartResults(nCycles, nValues);
  colnames(modelStartResults) = valueNames;
  rownames(modelStartResults) = make_cycle_rownames(1, nCycles);
  for (int j = 0; j < nValues; ++j) {
    modelStartResults(0, j) = modelStartVec[static_cast<size_t>(j)];
  }

  // ---------- Total values ----------
  NumericMatrix totalValues(nCycles, nValues);
  colnames(totalValues) = valueNames;
  rownames(totalValues) = make_cycle_rownames(1, nCycles);
  for (int i = 0; i < nCycles; ++i) {
    for (int j = 0; j < nValues; ++j) {
      totalValues(i, j) =
        transitionalValueResults(i, j) +
        residencyResults(i, j) +
        modelStartResults(i, j);
    }
  }

  return List::create(
    Named("trace") = trace,
    Named("uncondtransprod") = uncondTransProbs,
    Named("transitions") = transitions,
    Named("errors") = transitionErrors,
    Named("transitionalValues") = transitionalValueResults,
    Named("residencyValues") = residencyResults,
    Named("modelStartValues") = modelStartResults,
    Named("values") = totalValues
  );
}

// ============================================================================
// New Comprehensive cppCalculateTraceAndValues Function
// ============================================================================

// Helper to check if value is NA
inline bool isNA(SEXP x) {
  if (TYPEOF(x) == REALSXP && LENGTH(x) > 0) {
    return ISNA(REAL(x)[0]) || R_IsNA(REAL(x)[0]);
  }
  if (TYPEOF(x) == LGLSXP && LENGTH(x) > 0) {
    return LOGICAL(x)[0] == NA_LOGICAL;
  }
  if (TYPEOF(x) == STRSXP && LENGTH(x) > 0) {
    // Check if the string is NA_STRING
    return STRING_ELT(x, 0) == NA_STRING;
  }
  if (TYPEOF(x) == INTSXP && LENGTH(x) > 0) {
    return INTEGER(x)[0] == NA_INTEGER;
  }
  return false;
}

// Process transition errors and generate warning table in C++
void processAndWarnTransitionErrors(
    LogicalMatrix errors,
    NumericMatrix transitions,
    CharacterVector state_names,
    DataFrame expanded_state_map
) {
  const int nRows = errors.nrow();
  if (nRows == 0) return;

  // Error type mapping
  struct ErrorInfo {
    int cycle;
    std::string from_state;
    std::string to_state;
    int from_state_time;
    std::string error_type;
    bool is_state_level_error;
  };

  std::vector<ErrorInfo> all_errors;

  // Process each row for errors
  for (int r = 0; r < nRows; ++r) {
    int cycle = static_cast<int>(transitions(r, 0));
    int from_idx = static_cast<int>(transitions(r, 1));
    int to_idx = static_cast<int>(transitions(r, 2));

    std::string from_expanded = as<std::string>(state_names[from_idx - 1]);
    std::string to_expanded = as<std::string>(state_names[to_idx - 1]);

    StateNameParts from_parts = parseStateName(from_expanded);
    StateNameParts to_parts = parseStateName(to_expanded);

    // Check each error type
    if (errors(r, 0)) { // complement
      all_errors.push_back({
        cycle, from_parts.collapsed_name, "", from_parts.state_time,
        "Complement ('C') specified more than once for state/cycle", true
      });
    }
    if (errors(r, 1)) { // probLessThanZero
      all_errors.push_back({
        cycle, from_parts.collapsed_name, to_parts.collapsed_name, from_parts.state_time,
        "Transition probability less than 0", false
      });
    }
    if (errors(r, 2)) { // probGreaterThanOne
      all_errors.push_back({
        cycle, from_parts.collapsed_name, to_parts.collapsed_name, from_parts.state_time,
        "Transition probability greater than 1", false
      });
    }
    if (errors(r, 3)) { // sumNotEqualOne
      all_errors.push_back({
        cycle, from_parts.collapsed_name, "", from_parts.state_time,
        "Transition probabilities for state do not sum to 1", true
      });
    }
    if (errors(r, 4)) { // NaOrNaN
      all_errors.push_back({
        cycle, from_parts.collapsed_name, to_parts.collapsed_name, from_parts.state_time,
        "NA or NaN value found in transition probabilities", false
      });
    }
  }

  if (all_errors.empty()) return;

  // Consolidate errors by grouping
  struct ErrorGroup {
    std::string from_state;
    std::string to_state;
    std::string error_type;
    std::vector<int> cycles;
    std::vector<int> state_times;
    bool has_state_times;
  };

  std::map<std::string, ErrorGroup> error_groups;

  for (const auto& err : all_errors) {
    std::string key = err.from_state + "|" + err.to_state + "|" + err.error_type;

    if (error_groups.find(key) == error_groups.end()) {
      error_groups[key] = {
        err.from_state,
        err.to_state,
        err.error_type,
        {},
        {},
        false
      };
    }

    error_groups[key].cycles.push_back(err.cycle);
    error_groups[key].state_times.push_back(err.from_state_time);
    if (err.from_state_time > 1) {
      error_groups[key].has_state_times = true;
    }
  }

  // Check if any error has expanded states
  bool any_expanded = false;
  for (const auto& kv : error_groups) {
    if (kv.second.has_state_times) {
      any_expanded = true;
      break;
    }
  }

  // Build warning message table
  std::stringstream warning_msg;
  warning_msg << "Transition probability errors detected:\n\n";

  // Table header
  if (any_expanded) {
    warning_msg << "| From State | State Time | To State | Cycles | Error Message |\n";
    warning_msg << "|------------|------------|----------|--------|---------------|\n";
  } else {
    warning_msg << "| From State | To State | Cycles | Error Message |\n";
    warning_msg << "|------------|----------|--------|---------------|\n";
  }

  // Table rows (limit to 40)
  int row_count = 0;
  for (const auto& kv : error_groups) {
    if (row_count >= 40) {
      warning_msg << "\n... (showing top 40 of " << error_groups.size() << " errors)";
      break;
    }

    const ErrorGroup& grp = kv.second;
    std::string cycles_str = formatRanges(grp.cycles);
    std::string state_times_str = grp.has_state_times ? formatRanges(grp.state_times) : "N/A";

    warning_msg << "| " << grp.from_state;
    if (any_expanded) {
      warning_msg << " | " << state_times_str;
    }
    warning_msg << " | " << grp.to_state;
    warning_msg << " | " << cycles_str;
    warning_msg << " | " << grp.error_type << " |\n";

    row_count++;
  }

  // Issue warning
  std::string error_mode = "warning";
  Environment base_env = Environment::base_env();
  Function getOption = base_env["getOption"];
  SEXP mode_opt = getOption("openqaly.error_mode", "warning");
  if (mode_opt != R_NilValue && TYPEOF(mode_opt) == STRSXP && LENGTH(mode_opt) > 0) {
    error_mode = as<std::string>(mode_opt);
  }

  if (error_mode == "checkpoint") {
    stop(warning_msg.str());
  } else {
    Function warning_fn = base_env["warning"];
    warning_fn(warning_msg.str(), Named("call.") = false);
  }
}

// Transform transitions output with state name mappings
DataFrame transformTransitionsOutput(
    NumericMatrix transitions,
    CharacterVector state_names,
    DataFrame expanded_state_map
) {
  const int nRows = transitions.nrow();

  // Validate input dimensions
  if (nRows == 0 || transitions.ncol() < 4) {
    // Return empty DataFrame with correct structure
    return DataFrame::create(
      Named("cycle") = IntegerVector(0),
      Named("from_collapsed") = CharacterVector(0),
      Named("from_expanded") = CharacterVector(0),
      Named("to_collapsed") = CharacterVector(0),
      Named("to_expanded") = CharacterVector(0),
      Named("value") = NumericVector(0)
    );
  }

  // Extract expanded state map columns
  CharacterVector esm_from_state = expanded_state_map["from_state"];
  CharacterVector esm_from_e = expanded_state_map[".from_e"];

  // Build lookup map: expanded -> collapsed
  std::unordered_map<std::string, std::string> expanded_to_collapsed;
  for (int i = 0; i < esm_from_state.size(); ++i) {
    expanded_to_collapsed[as<std::string>(esm_from_e[i])] = as<std::string>(esm_from_state[i]);
  }

  // Create output vectors - all with the same size
  IntegerVector cycle_out(nRows);
  CharacterVector from_collapsed(nRows);
  CharacterVector from_expanded(nRows);
  CharacterVector to_collapsed(nRows);
  CharacterVector to_expanded(nRows);
  NumericVector value_out(nRows);

  // Process each row
  for (int r = 0; r < nRows; ++r) {
    cycle_out[r] = static_cast<int>(transitions(r, 0));
    value_out[r] = transitions(r, 3);

    int from_idx = static_cast<int>(transitions(r, 1)) - 1; // 1-based to 0-based
    int to_idx = static_cast<int>(transitions(r, 2)) - 1;

    // Bounds checking to avoid segfaults
    if (from_idx < 0 || from_idx >= state_names.size() ||
        to_idx < 0 || to_idx >= state_names.size()) {
      // Handle invalid indices
      from_expanded[r] = "INVALID";
      to_expanded[r] = "INVALID";
      from_collapsed[r] = "INVALID";
      to_collapsed[r] = "INVALID";
      continue;
    }

    std::string from_exp = as<std::string>(state_names[from_idx]);
    std::string to_exp = as<std::string>(state_names[to_idx]);

    from_expanded[r] = from_exp;
    to_expanded[r] = to_exp;

    // Lookup collapsed names
    auto from_it = expanded_to_collapsed.find(from_exp);
    if (from_it != expanded_to_collapsed.end()) {
      from_collapsed[r] = from_it->second;
    } else {
      from_collapsed[r] = from_exp; // fallback
    }

    auto to_it = expanded_to_collapsed.find(to_exp);
    if (to_it != expanded_to_collapsed.end()) {
      to_collapsed[r] = to_it->second;
    } else {
      to_collapsed[r] = to_exp; // fallback
    }
  }

  // Verify all vectors have the same length before creating DataFrame
  if (cycle_out.size() != nRows || from_collapsed.size() != nRows ||
      from_expanded.size() != nRows || to_collapsed.size() != nRows ||
      to_expanded.size() != nRows || value_out.size() != nRows) {
    stop("Internal error: vectors have different lengths in transformTransitionsOutput");
  }

  return DataFrame::create(
    Named("cycle") = cycle_out,
    Named("from_collapsed") = from_collapsed,
    Named("from_expanded") = from_expanded,
    Named("to_collapsed") = to_collapsed,
    Named("to_expanded") = to_expanded,
    Named("value") = value_out
  );
}

// [[Rcpp::export]]
List cppCalculateTraceAndValues(
    NumericVector init,
    NumericMatrix transitions,
    DataFrame values,
    CharacterVector value_names,
    CharacterVector state_names,  // Add expanded state names parameter
    DataFrame expanded_state_map,
    std::string half_cycle_method = "start"
) {
  // Extract number of cycles
  int n_cycles = 0;
  for (int i = 0; i < transitions.nrow(); ++i) {
    int cycle = static_cast<int>(transitions(i, 0));
    if (cycle > n_cycles) n_cycles = cycle;
  }

  // state_names now comes as a parameter, no need to extract from init

  // Filter values into categories (replacing R's filter operations)
  std::vector<int> transitional_rows;
  std::vector<int> residency_rows;
  std::vector<int> model_start_rows;

  CharacterVector values_state = values["state"];
  CharacterVector values_destination = values["destination"];

  for (int i = 0; i < values.nrow(); ++i) {
    bool state_na = Rcpp::CharacterVector::is_na(values_state[i]);
    bool dest_na = Rcpp::CharacterVector::is_na(values_destination[i]);

    if (!state_na && !dest_na) {
      transitional_rows.push_back(i);
    } else if (!state_na && dest_na) {
      residency_rows.push_back(i);
    } else if (state_na && dest_na) {
      model_start_rows.push_back(i);
    }
  }

  // Create filtered DataFrames with complete type handling
  auto filterDataFrame = [](DataFrame df, const std::vector<int>& rows) -> DataFrame {
    if (rows.empty()) {
      // Return empty DataFrame with same structure
      CharacterVector names = df.names();
      List cols;
      for (int i = 0; i < names.size(); ++i) {
        SEXP col = df[i];
        SEXPTYPE col_type = TYPEOF(col);

        // Create empty vector of the same type
        switch(col_type) {
          case REALSXP:
            cols.push_back(NumericVector(0));
            break;
          case INTSXP:
            cols.push_back(IntegerVector(0));
            break;
          case STRSXP:
            cols.push_back(CharacterVector(0));
            break;
          case LGLSXP:
            cols.push_back(LogicalVector(0));
            break;
          case VECSXP:
            cols.push_back(List(0));
            break;
          default:
            cols.push_back(R_NilValue);
            break;
        }
      }

      // Create empty DataFrame properly
      List df_list;
      for (int i = 0; i < cols.size(); ++i) {
        df_list.push_back(cols[i]);
      }
      df_list.attr("names") = names;
      df_list.attr("row.names") = IntegerVector::create(NA_INTEGER, 0);
      df_list.attr("class") = "data.frame";

      DataFrame result(df_list);
      return result;
    }

    List cols;
    CharacterVector names = df.names();

    // Process each column with complete type coverage
    for (int j = 0; j < df.size(); ++j) {
      SEXP col = df[j];
      SEXPTYPE col_type = TYPEOF(col);

      switch(col_type) {
        case REALSXP: {
          NumericVector orig = col;
          NumericVector filtered(rows.size());
          for (size_t i = 0; i < rows.size(); ++i) filtered[i] = orig[rows[i]];
          cols.push_back(filtered);
          break;
        }
        case INTSXP: {
          // IntegerVector can be a factor or have other attributes
          // We need to preserve all attributes
          IntegerVector orig = col;
          IntegerVector filtered(rows.size());
          for (size_t i = 0; i < rows.size(); ++i) {
            if (rows[i] >= 0 && rows[i] < orig.size()) {
              filtered[i] = orig[rows[i]];
            } else {
              filtered[i] = NA_INTEGER;
            }
          }
          // Copy all attributes (for factors, etc.)
          SEXP orig_attribs = ATTRIB(col);
          if (orig_attribs != R_NilValue) {
            SET_ATTRIB(filtered, orig_attribs);
          }
          cols.push_back(filtered);
          break;
        }
        case STRSXP: {
          CharacterVector orig = col;
          CharacterVector filtered(rows.size());
          for (size_t i = 0; i < rows.size(); ++i) filtered[i] = orig[rows[i]];
          cols.push_back(filtered);
          break;
        }
        case VECSXP: {
          List orig = col;
          List filtered(rows.size());
          for (size_t i = 0; i < rows.size(); ++i) filtered[i] = orig[rows[i]];
          cols.push_back(filtered);
          break;
        }
        default: {
          stop("Unsupported column type in filterDataFrame. Please report this issue.");
        }
      }
    }

    // Use DataFrame::create for more robust construction
    DataFrame result;
    if (cols.size() == names.size() && cols.size() > 0) {
      // Build the DataFrame piece by piece
      List df_list;
      for (int i = 0; i < cols.size(); ++i) {
        df_list.push_back(cols[i]);
      }
      df_list.attr("names") = names;
      df_list.attr("row.names") = IntegerVector::create(NA_INTEGER, -static_cast<int>(rows.size()));
      df_list.attr("class") = "data.frame";
      result = DataFrame(df_list);
    } else {
      result = DataFrame(cols);
      result.names() = names;
    }

    return result;
  };

  DataFrame transitional_values = filterDataFrame(values, transitional_rows);
  DataFrame residency_values = filterDataFrame(values, residency_rows);
  DataFrame model_start_values = filterDataFrame(values, model_start_rows);

  // Call the optimized cppMarkovTransitionsAndTrace
  List trace_transitions_values = cppMarkovTransitionsAndTrace(
    transitions,
    transitional_values,
    residency_values,
    model_start_values,
    init,
    state_names,
    value_names,
    n_cycles,
    -M_PI, // complementConstant
    half_cycle_method
  );

  // Process errors and issue warnings if needed
  LogicalMatrix errors = trace_transitions_values["errors"];
  bool has_errors = false;
  for (int r = 0; r < errors.nrow(); ++r) {
    for (int c = 0; c < errors.ncol(); ++c) {
      if (errors(r, c)) {
        has_errors = true;
        break;
      }
    }
    if (has_errors) break;
  }

  if (has_errors) {
    processAndWarnTransitionErrors(errors, transitions, state_names, expanded_state_map);
  }

  // Transform transitions output with state name mappings
  NumericMatrix output_trans = trace_transitions_values["transitions"];

  // Check if the dimensions are valid
  if (output_trans.nrow() > 0 && output_trans.ncol() >= 4) {
    DataFrame transformed_trans = transformTransitionsOutput(output_trans, state_names, expanded_state_map);
    // Update the result with transformed transitions
    trace_transitions_values["transitions"] = transformed_trans;
  } else {
    // Return the original matrix if transformation isn't possible
    trace_transitions_values["transitions"] = output_trans;
  }

  return trace_transitions_values;
}

