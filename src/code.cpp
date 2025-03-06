#include <Rcpp.h>
using namespace Rcpp;
bool useDebug = false;

void DebugPrintText(std::string text) {
    if (useDebug) {
        Rcout << text << "\n";
    }
}
void DebugPrintValue(std::string label, double value) {
    if (useDebug) {
        Rcout << (label + ": ") << value << "\n";
    }
}

// [[Rcpp::export]]
List MarkovTraceAndValues(NumericMatrix transitions, List values, NumericVector init, int ncycles, int nstates, int nvalues, double ccons) {
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
;
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

/*


*/
// [[Rcpp::export]]
List cppMarkovTransitionsAndTrace(
    NumericMatrix transitions,
    DataFrame valuesTransitional,
    NumericVector initialProbs,
    CharacterVector stateNames,
    CharacterVector valueNames,
    int nCycles,
    double complementConstant
) {

    int nValues = valueNames.length();
    int nStates = stateNames.length();

    // Create a dictionary of transitional values
    std::map<String,List> transitionalValueDictionary;
    int transitionalValueRows = valuesTransitional.nrow();
    Rcpp::List valCol;
    Rcpp::CharacterVector stateCol;
    Rcpp::CharacterVector destCol;
    Rcpp::List valList;
    int nTransitionalValueInstances = 0;
    for (int i = 0; i < transitionalValueRows; i++) {
        stateCol = valuesTransitional["state"];
        destCol = valuesTransitional["destination"];
        valCol = valuesTransitional["values_list"];
        std::string fromState = std::string(stateCol[i]);
        std::string destState = std::string(destCol[i]);
        std::string mapKey = fromState + "+" + destState;
        //Rcout << mapKey << "\n";
        transitionalValueDictionary[mapKey] = valCol[i];
        nTransitionalValueInstances++;
    }

    Rcpp::NumericMatrix transValueRes(nCycles * nTransitionalValueInstances, 4);

    // Set up a few R functions to call
    Environment base = Environment::namespace_env("base");
    Function seq = base["seq"];
    Function asCharacter = base["as.character"];

    int transRows = transitions.nrow();

    // Define matrix to store trace probabilities &
    // set row/column names.
    Rcpp::NumericMatrix trace(nCycles + 1, nStates);
    colnames(trace) = stateNames;
    rownames(trace) = asCharacter(
        seq(
            Named("from") = 0,
            Named("to") = nCycles,
            Named("by") = 1
        )
    );

    // Define a data frame to store the transitional values
    int transitionValueRows = nTransitionalValueInstances * nCycles;
    // Rcout << "Row count: " << (transitionValueRows) << "\n";

    IntegerVector tvalCycleCol = IntegerVector(transitionValueRows);
    CharacterVector tvalStateCol = CharacterVector(transitionValueRows);
    CharacterVector tvalDestCol = CharacterVector(transitionValueRows);
    NumericVector tvalValCol = NumericVector(transitionValueRows);
    DataFrame transitionalValueResults = DataFrame::create(
        Named("cycle") = tvalCycleCol,
        Named("state") = tvalStateCol,
        Named("destination") = tvalDestCol,
        Named("value") = tvalValCol
    );

    // Define matrix to store the table of unconditional transition probabilities
    Rcpp::NumericMatrix uncondTransProbs(transRows, 4); // Unconditional probabilities of transtitions
    colnames(uncondTransProbs) = CharacterVector::create("cycle", "from", "to", "value");

    Rcpp::LogicalMatrix transitionErrors(transRows, 4); // Store errors related to transition matrix,
    colnames(transitionErrors) = CharacterVector::create("complement", "outsideBounds", "sumNotEqualOne", "NaOrNaN");
    // columns: ComplementErrors, OutsideBoundsErrors, SumNotEqualOneErrors, NAOrNaNError

    DebugPrintValue("Number of rows", trace.nrow());
    DebugPrintValue("Number of columns", trace.ncol());

    // Populate first row of trace with initial state probabiltiies
    for(int i = 0; i < nStates; i++) {
        trace(0, i) = initialProbs[i];
    }


    int transrows = transitions.nrow();
    int currentTransitionsRow = 0;
    int toState;
    double value;
    bool doneWithCurrentState = false;
    int complementsFoundInState = 0;
    int complementRowIndex;
    double cumulativeProbability = 0; 
    double fromStateTraceProb;
    double uncondTransProb;
    int tvalRowIndex = 0;
    Rcpp::CharacterVector valNames;
    Rcpp::NumericVector valValues;

    // Loop through each cycle and from state
    for(int cycle = 1; cycle <= nCycles; cycle++) {
        DebugPrintText("______________________________________\n");
        DebugPrintValue("STARTING NEW CYCLE", cycle);
        DebugPrintText("______________________________________\n");
        for(int fromState = 0; fromState < nStates; fromState++) {
            DebugPrintText("    ______________________________________\n");
            DebugPrintValue("    STARTING NEW STATE", fromState);
            DebugPrintText("    ______________________________________\n");
            do {
                toState = int (transitions(currentTransitionsRow, 2)) - 1;
                value =  transitions(currentTransitionsRow, 3);
                DebugPrintValue("        CURRENT ROW", currentTransitionsRow);
                DebugPrintValue("        FROM", fromState);
                DebugPrintValue("        TO", toState);
                DebugPrintValue("        CYCLE", cycle);
                DebugPrintValue("        VALUE", value);

                if (value == complementConstant) {
                    // If we find a complementary value then take note of where it was and
                    // we will deal with it at the end of the state/cycle.
                    complementsFoundInState++;
                    complementRowIndex = currentTransitionsRow;
                    transitionErrors(currentTransitionsRow, 0) = complementsFoundInState > 1;
                } else {
                    // If it isn't a complementary probability than go ahead and add it
                    // to the cumulative probability and calculate/set the trace & unconditional
                    // transition probability.
                    cumulativeProbability += value;
                    DebugPrintValue("        CUMULATIVE PROBABILITY", cumulativeProbability);
                    fromStateTraceProb = trace(cycle - 1, fromState);
                    DebugPrintValue("        PROB OF BEING IN FROM STATE", fromStateTraceProb);
                    uncondTransProb = fromStateTraceProb * value;
                    DebugPrintValue("        UNCOND PROB OF TRANSITION", uncondTransProb);
                    trace(cycle, toState) += uncondTransProb;
                    DebugPrintValue("        TRACE SET TO", trace(cycle, toState));

                    // Populate row for unconditional transition probabilities
                    uncondTransProbs(currentTransitionsRow, 0) = cycle;
                    uncondTransProbs(currentTransitionsRow, 1) = fromState;
                    uncondTransProbs(currentTransitionsRow, 2) = toState;
                    uncondTransProbs(currentTransitionsRow, 3) = uncondTransProb;

                    // Set transitional values
                    std::string fromStateName = std::string(stateNames[fromState]);
                    std::string toStateName = std::string(stateNames[toState]);
                    valList = transitionalValueDictionary[fromStateName + "+" + toStateName];
                    int nVals = valList.length();
                    for (int valIndex = 0; valIndex < nVals; valIndex++) {
                        valNames = valList.names();
                        std::string valName = std::string(valNames[valIndex]);
                        valValues = valList[valIndex];
                        int nValueCycles = valValues.length();
                        double valValue = valValues[0];
                        if (nValueCycles > 1) {
                            valValue = valValues[cycle - 1];
                        }
                        
                        // Rcout << "From: " << (fromStateName) << "\n";
                        // Rcout << "To: " << (toStateName) << "\n";
                        // Rcout << "Cycle: " << (cycle) << "\n";
                        // Rcout << "Value value: " << (valValue) << "\n";
                        // Rcout << "Uncond Trans Prob: " << (uncondTransProb) << "\n";
                        // Rcout << "Value res: " << (uncondTransProb * valValue) << "\n";
                        // Rcout << "Setting trans value row: " << (tvalRowIndex) << "\n";
                        tvalCycleCol[tvalRowIndex] = cycle;
                        tvalStateCol[tvalRowIndex] = fromStateName;
                        tvalDestCol[tvalRowIndex] = toStateName;
                        tvalValCol[tvalRowIndex] = uncondTransProb * valValue;
                        // Rcout << "Set trans value row: " << (tvalRowIndex) << "\n";
                        tvalRowIndex++;
                    }

                    // Populate row for error tracking of transition probabilities.
                    transitionErrors(currentTransitionsRow, 0) = complementsFoundInState > 1;
                    transitionErrors(currentTransitionsRow, 1) = (value > 1) || (value < 0);
                    transitionErrors(currentTransitionsRow, 3) = std::isnan(value);
                }

                // Check if it is time to move to the next set of transitions
                currentTransitionsRow++;
                if (currentTransitionsRow >= transrows) {
                    doneWithCurrentState = true;
                } else {
                    int nextFromState = int (transitions(currentTransitionsRow, 1)) - 1;
                    int nextCycle = int (transitions(currentTransitionsRow, 0));
                    doneWithCurrentState = (nextFromState != fromState) || (nextCycle != cycle);
                }

                DebugPrintText("        - - - - - - - - - - -\n");
            } while (!doneWithCurrentState);

            // Calculate the complementary transition probability (if one was found),
            // update it in the transitions, then calculate/set the trace and unconditional
            // transition probability.
            if (complementsFoundInState > 0) {
                int complementToState = int(transitions(complementRowIndex, 2)) - 1;
                DebugPrintText("    HANDLING COMPLEMENTARY PROBABILITY");
                DebugPrintValue("    CUMULATIVE PROBABILITY", cumulativeProbability);
                double complementValue = 1 - cumulativeProbability;
                transitions(complementRowIndex, 3) = complementValue;
                fromStateTraceProb = trace(cycle - 1, fromState);
                DebugPrintValue("    PROB OF BEING IN FROM STATE", fromStateTraceProb);
                uncondTransProb = fromStateTraceProb * complementValue;
                DebugPrintValue("    UNCOND PROB OF TRANSITION", uncondTransProb);
                trace(cycle, complementToState) += uncondTransProb;
                DebugPrintValue("    TRACE SET TO", trace(cycle, complementToState));

                // Set unconditional transition probabilities
                uncondTransProbs(complementRowIndex, 0) = cycle;
                uncondTransProbs(complementRowIndex, 1) = fromState;
                uncondTransProbs(complementRowIndex, 2) = complementToState;
                uncondTransProbs(complementRowIndex, 3) = uncondTransProb;

                // Set transitional values
                std::string fromStateName = std::string(stateNames[fromState]);
                std::string toStateName = std::string(stateNames[complementToState]);
                valList = transitionalValueDictionary[fromStateName + "+" + toStateName];
                //Rcout << "Value list length: " << (fromStateName + "+" + toStateName) << "\n";
                //Rcout << "Value list length: " << valList.length() << "\n";


                transitionErrors(complementRowIndex, 1) = (complementValue > 1) || (complementValue < 0);
                transitionErrors(complementRowIndex, 3) = std::isnan(complementValue);
            } else {
                // Rcout << "Cumulative prob no complement: " << cumulativeProbability << "\n";
                // Rcout << "Cumulative prob equals 1: " << (cumulativeProbability == 1) << "\n";
                transitionErrors(currentTransitionsRow - 1, 2) = cumulativeProbability != 1;
            }

            // Reset everything for move to next set of transitions
            complementsFoundInState = 0;
            cumulativeProbability = 0;
        }

        //String stateName = stateNames[fromState]

        // Keep an array of uncond trans prods and trace for each cycle
        // then here calculate transitional and residence values for that
        // state/cycle.
    }

    return List::create(
        Named("trace") = trace,
        Named("uncondtransprod") = uncondTransProbs,
        Named("transitions") = transitions,
        Named("errors") = transitionErrors,
        Named("transitionalValues") = transitionalValueResults
    );
}