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
// Old MarkovTraceAndValues function (kept for backward compatibility)
// ============================================================================

// [[Rcpp::export]]
List MarkovTraceAndValues(NumericMatrix transitions, List values, NumericVector init,
                         int ncycles, int nstates, int nvalues, double ccons) {
    Rcpp::NumericMatrix trace(ncycles + 1, nstates);
    Rcpp::NumericVector results(ncycles * nstates * nvalues);

    std::map<String,double> myMap;
    myMap["1_1_1"] = 4.3;
    myMap["1_1_2"] = 199.3;

    DebugPrintValue("Number of rows", trace.nrow());
    DebugPrintValue("Number of columns", trace.ncol());
    int transrows = transitions.nrow();
    Rcpp::NumericMatrix uncondtransprod(transrows, 4);
    //Rcpp::NumericMatrix outcomes();

    for(int i = 0; i < nstates; i++) {
        trace(0, i) = init[i];
    }

    int from;
    int last_from;
    int to;
    int cycle;
    double value;
    double prevtracevalue;
    double currtracevalue;
    double cumtransprod = 0;
    int cconsfound = 0;

    for (int i = 0; i < transrows; i++) {
        from = int (transitions(i, 1)) - 1;
        cycle = int (transitions(i, 0));
        prevtracevalue = trace(cycle - 1, from);
        value = transitions(i, 3);
        DebugPrintValue("Row", i);
        if ((i > 0) && (from != last_from)) {
            DebugPrintValue("New From State", from);
            cumtransprod = value;
            cconsfound = 0;
        } else {
            if (value == ccons) {
                value = 1 - cumtransprod;
                transitions(i, 3) = value;
                cconsfound++;
                DebugPrintText("Found C");
                if (cconsfound > 1) {
                    stop("C may only be used once per from state");
                }
            } else {
                DebugPrintValue("Adding to cumulative", value);
                cumtransprod += value;
                if (cumtransprod > 1) {
                    stop("transition probabilities may not sum to >1");
                }
            }
        }

        DebugPrintText("________________________________\n");
        to = int (transitions(i, 2)) - 1;

        DebugPrintValue("From State", from);
        DebugPrintValue("To State", to);

        currtracevalue = trace(cycle, to);
        DebugPrintValue("Row to set", cycle);
        DebugPrintValue("Col to set", to);
        DebugPrintValue("Trans prob", value);
        DebugPrintValue("From state prob", prevtracevalue);
        DebugPrintValue("To state prob before this trans", currtracevalue);
        DebugPrintValue("Setting Value", (currtracevalue + (value * prevtracevalue)));
        double tracecomponent = value * prevtracevalue;
        uncondtransprod(i, 0) = cycle;
        uncondtransprod(i, 1) = to;
        uncondtransprod(i, 2) = from;
        uncondtransprod(i, 3) = tracecomponent;
        trace(cycle, to) = currtracevalue + tracecomponent;

        last_from = from;
    }

    //int index = 0;
    NumericVector valuevector;
    List valueslist;
    for (int i = 0; i < nstates; i++) {
        valueslist = values(i);
        if (valueslist.length() != 0) {
            for (int j = 0; j < nvalues; j++) {
                valuevector = valueslist(j);
                int valuevectorlength = valuevector.length();
                double valuevalue = valuevector(0);
                for (int k = 0; k < ncycles; k++) {
                    if (valuevectorlength > 1) {
                        valuevalue = valuevector(k);
                    }
                    results(k + i * ncycles + j * (ncycles * nstates)) = (trace(k, i) + trace(k + 1, i)) / 2 * valuevalue;
                }
            }
        }
    }

    IntegerVector dim = {ncycles, nstates, nvalues};
    results.attr("dim") = dim;

    return List::create(trace, uncondtransprod, results, transitions);
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
        case LGLSXP: {
          LogicalVector orig = col;
          LogicalVector filtered(rows.size());
          for (size_t i = 0; i < rows.size(); ++i) filtered[i] = orig[rows[i]];
          cols.push_back(filtered);
          break;
        }
        case RAWSXP: {
          RawVector orig = col;
          RawVector filtered(rows.size());
          for (size_t i = 0; i < rows.size(); ++i) filtered[i] = orig[rows[i]];
          cols.push_back(filtered);
          break;
        }
        case CPLXSXP: {
          ComplexVector orig = col;
          ComplexVector filtered(rows.size());
          for (size_t i = 0; i < rows.size(); ++i) filtered[i] = orig[rows[i]];
          cols.push_back(filtered);
          break;
        }
        default: {
          // For any other type (factors, dates, POSIXct, etc.), use generic SEXP handling
          // This preserves all attributes and handles special R types
          Shield<SEXP> filtered(Rf_allocVector(col_type, rows.size()));

          // Copy elements based on the underlying type
          if (col_type == VECSXP || col_type == EXPRSXP) {
            // For generic vectors and expressions
            for (size_t i = 0; i < rows.size(); ++i) {
              SET_VECTOR_ELT(filtered, i, VECTOR_ELT(col, rows[i]));
            }
          } else {
            // For other types, try to copy directly
            // This handles factors (which are INTSXP with attributes) and other special types
            stop("Unsupported column type in filterDataFrame. Please report this issue.");
          }

          // Copy all attributes to preserve factor levels, class, etc.
          DUPLICATE_ATTRIB(filtered, col);
          cols.push_back(filtered);
          break;
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

// ============================================================================
// Optimized evaluate_values C++ Implementation
// ============================================================================

// Helper struct to store NA information
struct NAInfo {
  std::string value_name;
  std::string state;
  std::string destination;
  std::vector<int> cycles;
  std::vector<int> state_cycles;
};

// Convert wide format to long format (replaces pivot_longer)
// [[Rcpp::export]]
List cpp_pivot_to_long(DataFrame df, CharacterVector value_names) {
  int n_rows = df.nrow();
  int n_values = value_names.size();

  // Get cycle and state_cycle columns
  IntegerVector cycle = df["cycle"];
  IntegerVector state_cycle = df["state_cycle"];

  // Create output vectors
  IntegerVector out_cycle(n_rows * n_values);
  IntegerVector out_state_cycle(n_rows * n_values);
  CharacterVector out_variable(n_rows * n_values);
  NumericVector out_value(n_rows * n_values);

  int out_idx = 0;
  for (int i = 0; i < n_rows; ++i) {
    for (int j = 0; j < n_values; ++j) {
      std::string var_name = as<std::string>(value_names[j]);

      // Check if this value column exists in the dataframe
      if (df.containsElementNamed(var_name.c_str())) {
        NumericVector col = df[var_name];
        out_cycle[out_idx] = cycle[i];
        out_state_cycle[out_idx] = state_cycle[i];
        out_variable[out_idx] = var_name;
        out_value[out_idx] = col[i];
        out_idx++;
      }
    }
  }

  // Resize to actual size
  out_cycle = out_cycle[Range(0, out_idx - 1)];
  out_state_cycle = out_state_cycle[Range(0, out_idx - 1)];
  out_variable = out_variable[Range(0, out_idx - 1)];
  out_value = out_value[Range(0, out_idx - 1)];

  return List::create(
    Named("cycle") = out_cycle,
    Named("state_cycle") = out_state_cycle,
    Named("variable") = out_variable,
    Named("value") = out_value
  );
}

// Convert long format to array (replaces lf_to_arr)
// [[Rcpp::export]]
NumericVector cpp_lf_to_array(List lf_data, IntegerVector dims) {
  IntegerVector cycle = lf_data["cycle"];
  IntegerVector state_cycle = lf_data["state_cycle"];
  CharacterVector variable = lf_data["variable"];
  NumericVector value = lf_data["value"];

  int n_cycles = dims[0];
  int n_state_cycles = dims[1];
  int n_variables = dims[2];

  // Create output array
  NumericVector arr(n_cycles * n_state_cycles * n_variables);
  std::fill(arr.begin(), arr.end(), 0.0);

  // Create variable index map
  CharacterVector unique_vars = unique(variable);
  std::unordered_map<std::string, int> var_idx;
  for (int i = 0; i < unique_vars.size(); ++i) {
    var_idx[as<std::string>(unique_vars[i])] = i;
  }

  // Fill array
  for (int i = 0; i < cycle.size(); ++i) {
    int c = cycle[i] - 1; // Convert to 0-based
    int sc = state_cycle[i] - 1;
    int v = var_idx[as<std::string>(variable[i])];

    int idx = c + sc * n_cycles + v * (n_cycles * n_state_cycles);
    arr[idx] = value[i];
  }

  // Set dimensions
  arr.attr("dim") = IntegerVector::create(n_cycles, n_state_cycles, n_variables);

  return arr;
}

// Find last unique value in array (replaces arr_last_unique)
// [[Rcpp::export]]
int cpp_arr_last_unique(NumericVector arr, int dim_index) {
  IntegerVector dims = arr.attr("dim");
  int n_dims = dims.size();

  if (dim_index < 1 || dim_index > n_dims) {
    stop("Invalid dimension index");
  }

  // Adjust for 0-based indexing
  dim_index = dim_index - 1;

  // Calculate strides
  std::vector<int> strides(n_dims);
  strides[0] = 1;
  for (int i = 1; i < n_dims; ++i) {
    strides[i] = strides[i-1] * dims[i-1];
  }

  int dim_size = dims[dim_index];

  // Get last slice
  std::vector<double> last_slice;
  int total_elements = 1;
  for (int i = 0; i < n_dims; ++i) {
    if (i != dim_index) {
      total_elements *= dims[i];
    }
  }
  last_slice.reserve(total_elements);

  // Extract last slice
  for (int i = 0; i < arr.size(); ++i) {
    int coord = (i / strides[dim_index]) % dims[dim_index];
    if (coord == dim_size - 1) {
      last_slice.push_back(arr[i]);
    }
  }

  // Check each slice for differences
  for (int slice_idx = dim_size - 2; slice_idx >= 0; --slice_idx) {
    bool different = false;
    int slice_offset = 0;

    for (int i = 0; i < arr.size(); ++i) {
      int coord = (i / strides[dim_index]) % dims[dim_index];
      if (coord == slice_idx) {
        if (!NumericVector::is_na(arr[i]) && !NumericVector::is_na(last_slice[slice_offset])) {
          if (std::abs(arr[i] - last_slice[slice_offset]) > 1e-10) {
            different = true;
            break;
          }
        }
        slice_offset++;
      }
    }

    if (different) {
      return slice_idx + 2; // Convert back to 1-based and add 1
    }
  }

  return 1;
}

// Main C++ function for processing evaluated values
// [[Rcpp::export]]
List process_evaluated_values_cpp(
    List evaluated_groups,
    CharacterVector value_names,
    CharacterVector state_names,
    bool simplify
) {
  int n_groups = evaluated_groups.size();

  // Storage for results and NA reports
  std::vector<List> all_results;
  std::vector<NAInfo> all_na_info;

  // Process each group
  for (int g = 0; g < n_groups; ++g) {
    List group = evaluated_groups[g];
    List group_info = group["group_info"];
    DataFrame evaluated_df = as<DataFrame>(group["evaluated_df"]);
    List evaluated_env = group["evaluated_env"];

    std::string state = as<std::string>(group_info["state"]);

    // Handle potential NA in destination
    std::string destination;
    SEXP dest_sexp = group_info["destination"];
    if (Rf_isString(dest_sexp) && LENGTH(dest_sexp) > 0) {
      if (STRING_ELT(dest_sexp, 0) != NA_STRING) {
        destination = as<std::string>(dest_sexp);
      } else {
        destination = "NA"; // Use "NA" as string representation
      }
    } else {
      destination = "NA";
    }

    double max_st = as<double>(group_info["max_st"]);

    // Check for NA values in evaluated columns
    CharacterVector df_names = evaluated_df.names();
    std::vector<std::string> value_cols_in_df;

    for (int i = 0; i < value_names.size(); ++i) {
      std::string vname = as<std::string>(value_names[i]);
      for (int j = 0; j < df_names.size(); ++j) {
        if (vname == as<std::string>(df_names[j])) {
          value_cols_in_df.push_back(vname);
          break;
        }
      }
    }

    // NA detection
    IntegerVector cycle_col = evaluated_df["cycle"];
    IntegerVector state_cycle_col = evaluated_df["state_cycle"];

    for (const auto& vname : value_cols_in_df) {
      NumericVector col = evaluated_df[vname];
      std::vector<int> na_cycles;
      std::vector<int> na_state_cycles;

      for (int i = 0; i < col.size(); ++i) {
        if (NumericVector::is_na(col[i])) {
          na_cycles.push_back(cycle_col[i]);
          na_state_cycles.push_back(state_cycle_col[i]);
        }
      }

      if (!na_cycles.empty()) {
        NAInfo na_info;
        na_info.value_name = vname;
        na_info.state = state;
        na_info.destination = destination;
        na_info.cycles = na_cycles;
        na_info.state_cycles = na_state_cycles;
        all_na_info.push_back(na_info);
      }
    }

    // Process simplify logic if needed
    if (simplify && !value_cols_in_df.empty() && evaluated_df.nrow() > 0) {
      // Convert to long format
      CharacterVector cols_to_pivot(value_cols_in_df.size());
      for (size_t i = 0; i < value_cols_in_df.size(); ++i) {
        cols_to_pivot[i] = value_cols_in_df[i];
      }

      List long_data = cpp_pivot_to_long(evaluated_df, cols_to_pivot);

      // Get unique cycles and state_cycles for dimensions
      IntegerVector cycles = long_data["cycle"];
      IntegerVector state_cycles = long_data["state_cycle"];
      IntegerVector unique_cycles = unique(cycles);
      IntegerVector unique_state_cycles = unique(state_cycles);

      // Create array
      IntegerVector dims = IntegerVector::create(
        unique_cycles.size(),
        unique_state_cycles.size(),
        cols_to_pivot.size()
      );

      NumericVector arr = cpp_lf_to_array(long_data, dims);

      // Find last unique
      int last_unique = cpp_arr_last_unique(arr, 2);
      max_st = std::min(max_st, static_cast<double>(last_unique));
    }

    // Add state column to evaluated_df
    CharacterVector state_col(evaluated_df.nrow(), state);
    evaluated_df["state"] = state_col;

    // Get value names in df and env
    std::vector<std::string> value_names_in_env;
    CharacterVector env_names = evaluated_env.names();

    for (int i = 0; i < value_names.size(); ++i) {
      std::string vname = as<std::string>(value_names[i]);
      for (int j = 0; j < env_names.size(); ++j) {
        if (vname == as<std::string>(env_names[j])) {
          value_names_in_env.push_back(vname);
          break;
        }
      }
    }

    // Group by state_cycle and create result structure
    std::map<int, std::vector<int>> state_cycle_groups;
    for (int i = 0; i < state_cycle_col.size(); ++i) {
      state_cycle_groups[state_cycle_col[i]].push_back(i);
    }

    // Process each state_cycle group
    for (const auto& sc_group : state_cycle_groups) {
      int state_cycle = sc_group.first;
      const std::vector<int>& indices = sc_group.second;

      // Create values_list for this state_cycle
      List values_list;

      // Add values from dataframe
      for (const auto& vname : value_cols_in_df) {
        NumericVector col = evaluated_df[vname];
        NumericVector val_vec(indices.size());
        for (size_t i = 0; i < indices.size(); ++i) {
          val_vec[i] = col[indices[i]];
        }
        values_list[vname] = val_vec;
      }

      // Add values from environment
      for (const auto& vname : value_names_in_env) {
        values_list[vname] = evaluated_env[vname];
      }

      // Create result row
      List result_row = List::create(
        Named("state") = state,
        Named("destination") = destination == "NA" ? NA_STRING : wrap(destination),
        Named("max_st") = max_st,
        Named("state_cycle") = state_cycle,
        Named("values_list") = List::create(values_list)
      );

      all_results.push_back(result_row);
    }
  }

  // Check for NA values and prepare error if needed
  if (!all_na_info.empty()) {
    // Consolidate NA information
    std::map<std::string, NAInfo> consolidated_na;

    for (const auto& na : all_na_info) {
      std::string key = na.value_name + "|" + na.state + "|" + na.destination;
      if (consolidated_na.find(key) == consolidated_na.end()) {
        consolidated_na[key] = na;
      } else {
        // Merge cycles and state_cycles
        consolidated_na[key].cycles.insert(
          consolidated_na[key].cycles.end(),
          na.cycles.begin(),
          na.cycles.end()
        );
        consolidated_na[key].state_cycles.insert(
          consolidated_na[key].state_cycles.end(),
          na.state_cycles.begin(),
          na.state_cycles.end()
        );
      }
    }

    // Build error message
    std::stringstream error_msg;
    error_msg << "NA values detected in evaluated model values:\n\n";
    error_msg << "| Value Name | State | Destination | Cycles | State Cycles |\n";
    error_msg << "|------------|-------|-------------|--------|-------------|\n";

    for (const auto& kv : consolidated_na) {
      const NAInfo& info = kv.second;
      error_msg << "| " << info.value_name;
      error_msg << " | " << info.state;
      error_msg << " | " << (info.destination == "NA" ? "N/A" : info.destination);
      error_msg << " | " << formatRanges(info.cycles);
      error_msg << " | " << formatRanges(info.state_cycles);
      error_msg << " |\n";
    }

    stop(error_msg.str());
  }

  // Combine all results into a DataFrame
  int n_results = all_results.size();

  CharacterVector out_state(n_results);
  CharacterVector out_destination(n_results);
  NumericVector out_max_st(n_results);
  IntegerVector out_state_cycle(n_results);
  List out_values_list(n_results);

  for (int i = 0; i < n_results; ++i) {
    List row = all_results[i];
    out_state[i] = as<std::string>(row["state"]);

    SEXP dest = row["destination"];
    if (TYPEOF(dest) == STRSXP && LENGTH(dest) > 0 && STRING_ELT(dest, 0) != NA_STRING) {
      out_destination[i] = as<std::string>(dest);
    } else {
      out_destination[i] = NA_STRING;
    }

    out_max_st[i] = as<double>(row["max_st"]);
    out_state_cycle[i] = as<int>(row["state_cycle"]);
    out_values_list[i] = as<List>(row["values_list"])[0];
  }

  // Create final DataFrame
  DataFrame result = DataFrame::create(
    Named("state") = out_state,
    Named("destination") = out_destination,
    Named("max_st") = out_max_st,
    Named("state_cycle") = out_state_cycle,
    Named("values_list") = out_values_list
  );

  // Sort by state factor levels (done in R)

  return result;
}

// ============================================================================
// Direct replacements for specific R bottleneck functions
// ============================================================================

// Ultrafast version using R's native subsetting
// [[Rcpp::export]]
DataFrame cpp_process_state_cycles_ultrafast(
    DataFrame state_res,
    CharacterVector value_names_in_df,
    List state_ns_env,
    CharacterVector value_names_in_env,
    std::string state,
    SEXP destination,  // Can be NA
    double max_st
) {
  // Get state_cycle column
  IntegerVector state_cycle_col = state_res["state_cycle"];
  int n_rows = state_cycle_col.size();

  // Use unordered_map for O(1) access
  std::unordered_map<int, std::vector<int>> state_cycle_groups;
  state_cycle_groups.reserve(n_rows / 2);

  for (int i = 0; i < n_rows; ++i) {
    state_cycle_groups[state_cycle_col[i]].push_back(i);
  }

  int n_groups = state_cycle_groups.size();

  // Pre-allocate output vectors
  CharacterVector out_state(n_groups, state);
  CharacterVector out_destination(n_groups);
  NumericVector out_max_st(n_groups, max_st);
  IntegerVector out_state_cycle(n_groups);
  List out_values_list(n_groups);

  // Handle destination
  if (TYPEOF(destination) == STRSXP && LENGTH(destination) > 0 &&
      STRING_ELT(destination, 0) != NA_STRING) {
    std::string dest_str = Rcpp::as<std::string>(destination);
    for (int i = 0; i < n_groups; ++i) {
      out_destination[i] = dest_str;
    }
  } else {
    for (int i = 0; i < n_groups; ++i) {
      out_destination[i] = NA_STRING;
    }
  }

  // Pre-extract all column SEXPs for faster access
  std::vector<SEXP> df_columns;
  std::vector<std::string> df_col_names;
  df_columns.reserve(value_names_in_df.size());
  df_col_names.reserve(value_names_in_df.size());

  for (int j = 0; j < value_names_in_df.size(); ++j) {
    std::string col_name = Rcpp::as<std::string>(value_names_in_df[j]);
    df_columns.push_back(state_res[col_name]);
    df_col_names.push_back(col_name);
  }

  // Pre-convert environment value names
  std::vector<std::string> env_val_names;
  env_val_names.reserve(value_names_in_env.size());
  for (int j = 0; j < value_names_in_env.size(); ++j) {
    env_val_names.push_back(Rcpp::as<std::string>(value_names_in_env[j]));
  }

  // Get R's subsetting function
  Environment base_env = Environment::base_env();
  Function subset_op = base_env["["];

  // Pre-build index vectors for all groups (1-based for R)
  std::vector<std::pair<int, IntegerVector>> group_indices;
  group_indices.reserve(n_groups);

  for (const auto& kv : state_cycle_groups) {
    IntegerVector r_idx(kv.second.size());
    for (size_t i = 0; i < kv.second.size(); ++i) {
      r_idx[i] = kv.second[i] + 1;  // Convert to 1-based
    }
    group_indices.push_back({kv.first, r_idx});
  }

  // Sort for better cache locality
  std::sort(group_indices.begin(), group_indices.end(),
            [](const auto& a, const auto& b) { return a.first < b.first; });

  // Process each group using R's native subsetting
  for (int group_idx = 0; group_idx < n_groups; ++group_idx) {
    int state_cycle = group_indices[group_idx].first;
    const IntegerVector& r_indices = group_indices[group_idx].second;

    out_state_cycle[group_idx] = state_cycle;

    // Create values list using R's fast subsetting
    List expanded_state_values_list;

    // Subset dataframe columns using R's [ operator
    for (size_t j = 0; j < df_columns.size(); ++j) {
      // Use R's native subsetting - much faster than manual copying
      SEXP subset = subset_op(df_columns[j], r_indices);
      expanded_state_values_list[df_col_names[j]] = subset;
    }

    // Add environment values
    for (const std::string& env_name : env_val_names) {
      expanded_state_values_list[env_name] = state_ns_env[env_name];
    }

    // Store as list with single element
    out_values_list[group_idx] = List::create(expanded_state_values_list);
  }

  return DataFrame::create(
    Named("state") = out_state,
    Named("destination") = out_destination,
    Named("max_st") = out_max_st,
    Named("state_cycle") = out_state_cycle,
    Named("values_list") = out_values_list,
    Named("stringsAsFactors") = false
  );
}

// Fast optimized replacement that returns a single DataFrame
// [[Rcpp::export]]
DataFrame cpp_process_state_cycles_fast(
    DataFrame state_res,
    CharacterVector value_names_in_df,
    List state_ns_env,
    CharacterVector value_names_in_env,
    std::string state,
    SEXP destination,  // Can be NA
    double max_st
) {
  // Get state_cycle column
  IntegerVector state_cycle_col = state_res["state_cycle"];
  int n_rows = state_cycle_col.size();

  // Use unordered_map for O(1) access and reserve capacity
  std::unordered_map<int, std::vector<int>> state_cycle_groups;
  state_cycle_groups.reserve(n_rows / 2); // Estimate

  for (int i = 0; i < n_rows; ++i) {
    state_cycle_groups[state_cycle_col[i]].push_back(i);
  }

  int n_groups = state_cycle_groups.size();

  // Pre-allocate all output vectors with exact size
  CharacterVector out_state(n_groups, state);
  CharacterVector out_destination(n_groups);
  NumericVector out_max_st(n_groups, max_st);
  IntegerVector out_state_cycle(n_groups);
  List out_values_list(n_groups);

  // Handle destination once (NA or string)
  bool dest_is_na = false;
  std::string dest_str;
  if (TYPEOF(destination) == STRSXP && LENGTH(destination) > 0) {
    if (STRING_ELT(destination, 0) != NA_STRING) {
      dest_str = Rcpp::as<std::string>(destination);
      for (int i = 0; i < n_groups; ++i) {
        out_destination[i] = dest_str;
      }
    } else {
      dest_is_na = true;
    }
  } else {
    dest_is_na = true;
  }
  if (dest_is_na) {
    for (int i = 0; i < n_groups; ++i) {
      out_destination[i] = NA_STRING;
    }
  }

  // Pre-determine column types to avoid repeated checking
  std::vector<SEXPTYPE> col_types(value_names_in_df.size());
  for (int j = 0; j < value_names_in_df.size(); ++j) {
    std::string col_name = Rcpp::as<std::string>(value_names_in_df[j]);
    SEXP col_sexp = state_res[col_name];
    col_types[j] = TYPEOF(col_sexp);
  }

  // Convert value names to strings once
  std::vector<std::string> df_col_names(value_names_in_df.size());
  for (int j = 0; j < value_names_in_df.size(); ++j) {
    df_col_names[j] = Rcpp::as<std::string>(value_names_in_df[j]);
  }

  std::vector<std::string> env_val_names(value_names_in_env.size());
  for (int j = 0; j < value_names_in_env.size(); ++j) {
    env_val_names[j] = Rcpp::as<std::string>(value_names_in_env[j]);
  }

  // Process groups - use sorted order for better memory locality
  std::vector<int> sorted_keys;
  sorted_keys.reserve(n_groups);
  for (const auto& kv : state_cycle_groups) {
    sorted_keys.push_back(kv.first);
  }
  std::sort(sorted_keys.begin(), sorted_keys.end());

  // Process each group
  for (int group_idx = 0; group_idx < n_groups; ++group_idx) {
    int state_cycle = sorted_keys[group_idx];
    const std::vector<int>& indices = state_cycle_groups[state_cycle];

    out_state_cycle[group_idx] = state_cycle;

    // Create expanded_state_values_list
    List expanded_state_values_list;

    // Add values from dataframe columns - optimized by type
    for (size_t j = 0; j < df_col_names.size(); ++j) {
      const std::string& col_name = df_col_names[j];
      SEXP col_sexp = state_res[col_name];

      switch(col_types[j]) {
        case REALSXP: {
          NumericVector col = Rcpp::as<NumericVector>(col_sexp);
          NumericVector subset_vals(indices.size());
          // Use pointer arithmetic for faster access
          double* col_ptr = &col[0];
          for (size_t k = 0; k < indices.size(); ++k) {
            subset_vals[k] = col_ptr[indices[k]];
          }
          expanded_state_values_list[col_name] = subset_vals;
          break;
        }
        case INTSXP: {
          IntegerVector col = Rcpp::as<IntegerVector>(col_sexp);
          IntegerVector subset_vals(indices.size());
          int* col_ptr = &col[0];
          for (size_t k = 0; k < indices.size(); ++k) {
            subset_vals[k] = col_ptr[indices[k]];
          }
          expanded_state_values_list[col_name] = subset_vals;
          break;
        }
        case LGLSXP: {
          LogicalVector col = Rcpp::as<LogicalVector>(col_sexp);
          LogicalVector subset_vals(indices.size());
          int* col_ptr = &col[0];
          for (size_t k = 0; k < indices.size(); ++k) {
            subset_vals[k] = col_ptr[indices[k]];
          }
          expanded_state_values_list[col_name] = subset_vals;
          break;
        }
        case STRSXP: {
          CharacterVector col = Rcpp::as<CharacterVector>(col_sexp);
          CharacterVector subset_vals(indices.size());
          for (size_t k = 0; k < indices.size(); ++k) {
            subset_vals[k] = col[indices[k]];
          }
          expanded_state_values_list[col_name] = subset_vals;
          break;
        }
        default: {
          // Fallback for other types
          NumericVector col = Rcpp::as<NumericVector>(col_sexp);
          NumericVector subset_vals(indices.size());
          for (size_t k = 0; k < indices.size(); ++k) {
            subset_vals[k] = col[indices[k]];
          }
          expanded_state_values_list[col_name] = subset_vals;
          break;
        }
      }
    }

    // Append values from environment (already efficient)
    for (const std::string& env_name : env_val_names) {
      expanded_state_values_list[env_name] = state_ns_env[env_name];
    }

    // Store as list with single element (required structure)
    out_values_list[group_idx] = List::create(expanded_state_values_list);
  }

  // Return single DataFrame instead of List of DataFrames
  return DataFrame::create(
    Named("state") = out_state,
    Named("destination") = out_destination,
    Named("max_st") = out_max_st,
    Named("state_cycle") = out_state_cycle,
    Named("values_list") = out_values_list,
    Named("stringsAsFactors") = false
  );
}

// Optimized replacement for the group_by/group_split/map operation in evaluate_values
// [[Rcpp::export]]
List cpp_process_state_cycles(
    DataFrame state_res,
    CharacterVector value_names_in_df,
    List state_ns_env,
    CharacterVector value_names_in_env,
    std::string state,
    SEXP destination,  // Can be NA
    double max_st
) {
  // Get state_cycle column
  IntegerVector state_cycle_col = state_res["state_cycle"];
  int n_rows = state_cycle_col.size();

  // Group rows by state_cycle using a map
  std::map<int, std::vector<int>> state_cycle_groups;
  for (int i = 0; i < n_rows; ++i) {
    state_cycle_groups[state_cycle_col[i]].push_back(i);
  }

  // Create list to hold tibbles (same as inner_mapped_rows)
  List result_list(state_cycle_groups.size());

  // Process each state_cycle group
  int group_idx = 0;
  for (const auto& sc_group : state_cycle_groups) {
    int state_cycle = sc_group.first;
    const std::vector<int>& indices = sc_group.second;

    // Create expanded_state_values_list
    List expanded_state_values_list;

    // Add values from dataframe columns (as.list(state_cycle_df[, value_names_in_df, drop = FALSE]))
    int n_value_cols = value_names_in_df.size();
    for (int j = 0; j < n_value_cols; ++j) {
      std::string col_name = Rcpp::as<std::string>(value_names_in_df[j]);

      // Get the column and subset it for this group
      SEXP col_sexp = state_res[col_name];

      if (TYPEOF(col_sexp) == REALSXP) {
        NumericVector col = Rcpp::as<NumericVector>(col_sexp);
        NumericVector subset_vals(indices.size());
        for (size_t k = 0; k < indices.size(); ++k) {
          subset_vals[k] = col[indices[k]];
        }
        expanded_state_values_list[col_name] = subset_vals;
      } else if (TYPEOF(col_sexp) == INTSXP) {
        IntegerVector col = Rcpp::as<IntegerVector>(col_sexp);
        IntegerVector subset_vals(indices.size());
        for (size_t k = 0; k < indices.size(); ++k) {
          subset_vals[k] = col[indices[k]];
        }
        expanded_state_values_list[col_name] = subset_vals;
      } else if (TYPEOF(col_sexp) == LGLSXP) {
        LogicalVector col = Rcpp::as<LogicalVector>(col_sexp);
        LogicalVector subset_vals(indices.size());
        for (size_t k = 0; k < indices.size(); ++k) {
          subset_vals[k] = col[indices[k]];
        }
        expanded_state_values_list[col_name] = subset_vals;
      } else if (TYPEOF(col_sexp) == STRSXP) {
        CharacterVector col = Rcpp::as<CharacterVector>(col_sexp);
        CharacterVector subset_vals(indices.size());
        for (size_t k = 0; k < indices.size(); ++k) {
          subset_vals[k] = col[indices[k]];
        }
        expanded_state_values_list[col_name] = subset_vals;
      } else {
        // Handle other types - default to numeric
        NumericVector col = Rcpp::as<NumericVector>(col_sexp);
        NumericVector subset_vals(indices.size());
        for (size_t k = 0; k < indices.size(); ++k) {
          subset_vals[k] = col[indices[k]];
        }
        expanded_state_values_list[col_name] = subset_vals;
      }
    }

    // Append values from environment (as.list(state_ns$env)[value_names_in_env])
    int n_env_values = value_names_in_env.size();
    for (int j = 0; j < n_env_values; ++j) {
      std::string env_name = Rcpp::as<std::string>(value_names_in_env[j]);
      expanded_state_values_list[env_name] = state_ns_env[env_name];
    }

    // Create the single-row tibble for this group
    // Note: values_list contains list(expanded_state_values_list) - a list with one element
    List values_list_wrapper = List::create(expanded_state_values_list);

    // Handle destination (can be NA)
    CharacterVector dest_vec(1);
    if (TYPEOF(destination) == STRSXP && LENGTH(destination) > 0) {
      if (STRING_ELT(destination, 0) != NA_STRING) {
        dest_vec[0] = Rcpp::as<std::string>(destination);
      } else {
        dest_vec[0] = NA_STRING;
      }
    } else if (TYPEOF(destination) == NILSXP ||
               (TYPEOF(destination) == LGLSXP && LOGICAL(destination)[0] == NA_LOGICAL)) {
      dest_vec[0] = NA_STRING;
    } else {
      // Try to convert to string
      dest_vec[0] = Rcpp::as<std::string>(destination);
    }

    // Create tibble for this group
    DataFrame group_tibble = DataFrame::create(
      Named("state") = CharacterVector::create(state),
      Named("destination") = dest_vec,
      Named("max_st") = NumericVector::create(max_st),
      Named("state_cycle") = IntegerVector::create(state_cycle),
      Named("values_list") = values_list_wrapper,
      Named("stringsAsFactors") = false
    );

    result_list[group_idx] = group_tibble;
    group_idx++;
  }

  return result_list;
}

// Direct replacement for lf_to_arr that works with dataframe directly
// [[Rcpp::export]]
NumericVector cpp_lf_to_array_direct(DataFrame df, CharacterVector dimcols, std::string value_col) {
  int n_dims = dimcols.size();
  int n_rows = df.nrow();

  // Get the value column
  NumericVector values = df[value_col];

  // Calculate dimensions and create factors
  IntegerVector lengths(n_dims);
  std::vector<IntegerVector> factors(n_dims);

  for (int i = 0; i < n_dims; ++i) {
    std::string col_name = as<std::string>(dimcols[i]);
    SEXP col = df[col_name];

    if (Rf_isFactor(col)) {
      // If it's already a factor, use it directly
      factors[i] = as<IntegerVector>(col);
      IntegerVector lvls = col;
      CharacterVector levels = lvls.attr("levels");
      lengths[i] = levels.size();
    } else {
      // Handle both numeric and character columns
      SEXPTYPE col_type = TYPEOF(col);

      if (col_type == INTSXP) {
        // Integer column
        IntegerVector col_int = as<IntegerVector>(df[col_name]);
        IntegerVector unique_vals = unique(col_int);
        lengths[i] = unique_vals.size();

        // Create factor indices
        IntegerVector factor_idx(n_rows);
        std::unordered_map<int, int> val_to_idx;
        for (int j = 0; j < unique_vals.size(); ++j) {
          val_to_idx[unique_vals[j]] = j + 1; // 1-based indexing
        }
        for (int j = 0; j < n_rows; ++j) {
          factor_idx[j] = val_to_idx[col_int[j]];
        }
        factors[i] = factor_idx;
      } else if (col_type == STRSXP) {
        // Character column
        CharacterVector col_char = as<CharacterVector>(df[col_name]);
        CharacterVector unique_vals = unique(col_char);
        lengths[i] = unique_vals.size();

        // Create factor indices
        IntegerVector factor_idx(n_rows);
        std::unordered_map<std::string, int> val_to_idx;
        for (int j = 0; j < unique_vals.size(); ++j) {
          val_to_idx[as<std::string>(unique_vals[j])] = j + 1; // 1-based indexing
        }
        for (int j = 0; j < n_rows; ++j) {
          factor_idx[j] = val_to_idx[as<std::string>(col_char[j])];
        }
        factors[i] = factor_idx;
      } else {
        // Try to convert to integer
        IntegerVector col_int = as<IntegerVector>(df[col_name]);
        IntegerVector unique_vals = unique(col_int);
        lengths[i] = unique_vals.size();

        IntegerVector factor_idx(n_rows);
        std::unordered_map<int, int> val_to_idx;
        for (int j = 0; j < unique_vals.size(); ++j) {
          val_to_idx[unique_vals[j]] = j + 1;
        }
        for (int j = 0; j < n_rows; ++j) {
          factor_idx[j] = val_to_idx[col_int[j]];
        }
        factors[i] = factor_idx;
      }
    }
  }

  // Calculate total size
  int total_size = 1;
  for (int i = 0; i < n_dims; ++i) {
    total_size *= lengths[i];
  }

  // Create output vector
  NumericVector vec(total_size);

  // Fill the array
  for (int row = 0; row < n_rows; ++row) {
    int index = 0;
    int multiplier = 1;

    for (int dim = 0; dim < n_dims; ++dim) {
      if (dim == 0) {
        index = factors[dim][row] - 1; // Convert to 0-based
      } else {
        index += (factors[dim][row] - 1) * multiplier;
      }
      if (dim < n_dims - 1) {
        multiplier *= lengths[dim];
      }
    }

    vec[index] = values[row];
  }

  // Set dimensions
  vec.attr("dim") = lengths;

  return vec;
}