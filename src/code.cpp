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
void DebugPrintStringValue(std::string label, std::string value) {
    if (useDebug) {
        Rcout << (label + ": ") << value << "\n";
    }
}

std::string getTransitionKey(std::string from, std::string to) {
    return from + "->" + to;
}


/*


*/
// [[Rcpp::export]]
List cppMarkovTransitionsAndTrace(
    DataFrame transitions,
    DataFrame valuesTransitional,
    NumericVector initialProbs,
    CharacterVector stateNames,
    CharacterVector valueNames,
    int nCycles,
    double complementConstant
) {

    // Set up a few R functions to call
    Environment base = Environment::namespace_env("base");
    Function seq = base["seq"];
    Function asCharacter = base["as.character"];

    // Get dimensions of various things to figure
    // out dimensions of return objects, number of iterations
    // for loops, etc...
    int nValues = valueNames.length();
    int nStates = stateNames.length();
    int transRows = transitions.nrow();
    int transitionalValueRows = valuesTransitional.nrow();
    int transitionValueRows = transitionalValueRows * nCycles;

    // Set up a fast way to look up the index
    // of states by the name of the state
    std::map<String,int> stateIndexDictionary;
    for (int i = 0; i < nStates; i ++) {
        stateIndexDictionary[stateNames[i]] = i;
    }

    // Extract columns from transitions data frame
    CharacterVector transFromCol = transitions[".from_e"];
    CharacterVector transToCol = transitions[".to_e"];
    IntegerVector transCycleCol = transitions["cycle"];
    NumericVector transValueCol = transitions["value"];

    // Set up a fast way to look up the list of transitional
    // values given a string containing the from state
    // and to state.
    std::map<String,List> transitionalValueDictionary;
    Rcpp::List valCol;
    Rcpp::CharacterVector stateCol;
    Rcpp::CharacterVector destCol;
    Rcpp::List valList;
    for (int i = 0; i < transitionalValueRows; i++) {
        stateCol = valuesTransitional["state"];
        destCol = valuesTransitional["destination"];
        valCol = valuesTransitional["values_list"];
        std::string fromState = std::string(stateCol[i]);
        std::string destState = std::string(destCol[i]);
        std::string mapKey = getTransitionKey(fromState, destState);
        transitionalValueDictionary[mapKey] = valCol[i];
    }

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
    // and be returned as part of a list.
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

    // Define a data frame to store the unconditional transition
    // probabilities (the % of patients undergoing each transition
    // in each cycle) and be returned as part of a list.
    IntegerVector utpCycleCol = IntegerVector(transRows);
    CharacterVector utpFromCol = CharacterVector(transRows);
    CharacterVector utpToCol = CharacterVector(transRows);
    NumericVector utpValCol = NumericVector(transRows);
    DataFrame utpResults = DataFrame::create(
        Named("cycle") = utpCycleCol,
        Named("from") = utpFromCol,
        Named("to") = utpToCol,
        Named("value") = utpValCol
    );


    // Define matrix to store the table of unconditional transition probabilities
    //Rcpp::NumericMatrix uncondTransProbs(transRows, 4); // Unconditional probabilities of transtitions
  //  colnames(uncondTransProbs) = CharacterVector::create("cycle", "from", "to", "value");

    Rcpp::LogicalMatrix transitionErrors(transRows, 4); // Store errors related to transition matrix,
    colnames(transitionErrors) = CharacterVector::create("complement", "outsideBounds", "sumNotEqualOne", "NaOrNaN");
    // columns: ComplementErrors, OutsideBoundsErrors, SumNotEqualOneErrors, NAOrNaNError

    DebugPrintValue("Number of rows", trace.nrow());
    DebugPrintValue("Number of columns", trace.ncol());

    // Populate first row of trace with initial state probabiltiies
    for(int i = 0; i < nStates; i++) {
        trace(0, i) = initialProbs[i];
    }

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
            std::string fromStateName = std::string(transFromCol[currentTransitionsRow]);
            do {

                std::string toStateName = std::string(transToCol[currentTransitionsRow]);
                toState = stateIndexDictionary[toStateName];
                value =  transValueCol[currentTransitionsRow];
                DebugPrintValue("        CURRENT ROW", currentTransitionsRow);
                DebugPrintStringValue("        FROM", fromStateName);
                DebugPrintValue("        FROM INDEX", fromState);
                DebugPrintStringValue("        TO", toStateName);
                DebugPrintValue("        TO INDEX", toState);
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
                    utpCycleCol[currentTransitionsRow] = cycle;
                    utpFromCol[currentTransitionsRow] = fromStateName;
                    utpToCol[currentTransitionsRow] = toStateName;
                    utpValCol[currentTransitionsRow] = uncondTransProb;

                    // Set transitional values
                    valList = transitionalValueDictionary[getTransitionKey(fromStateName,toStateName)];
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
                        tvalCycleCol[tvalRowIndex] = cycle;
                        tvalStateCol[tvalRowIndex] = fromStateName;
                        tvalDestCol[tvalRowIndex] = toStateName;
                        tvalValCol[tvalRowIndex] = uncondTransProb * valValue;
                        tvalRowIndex++;
                    }

                    // Populate row for error tracking of transition probabilities.
                    transitionErrors(currentTransitionsRow, 0) = complementsFoundInState > 1;
                    transitionErrors(currentTransitionsRow, 1) = (value > 1) || (value < 0);
                    transitionErrors(currentTransitionsRow, 3) = std::isnan(value);
                }

                // Check if it is time to move to the next set of transitions
                currentTransitionsRow++;
                if (currentTransitionsRow >= transRows) {
                    doneWithCurrentState = true;
                } else {
                    int nextFromState = stateIndexDictionary[transFromCol[currentTransitionsRow]];

                    int nextCycle = transCycleCol[currentTransitionsRow];
                    DebugPrintValue("    NEXT ROW FROM", nextFromState);
                    DebugPrintValue("    NEXT ROW CYCLE", nextCycle);
                    doneWithCurrentState = (nextFromState != fromState) || (nextCycle != cycle);
                }

                DebugPrintText("        - - - - - - - - - - -\n");
            } while (!doneWithCurrentState);

            // Calculate the complementary transition probability (if one was found),
            // update it in the transitions, then calculate/set the trace and unconditional
            // transition probability.
            if (complementsFoundInState > 0) {
                std::string complementToStateName = std::string(transToCol[complementRowIndex]);
                int complementToState = stateIndexDictionary[complementToStateName];
                DebugPrintText("    HANDLING COMPLEMENTARY PROBABILITY");
                DebugPrintValue("    CUMULATIVE PROBABILITY", cumulativeProbability);
                double complementValue = 1 - cumulativeProbability;
                transValueCol[complementRowIndex] = complementValue;
                fromStateTraceProb = trace(cycle - 1, fromState);
                DebugPrintValue("    PROB OF BEING IN FROM STATE", fromStateTraceProb);
                uncondTransProb = fromStateTraceProb * complementValue;
                DebugPrintValue("    UNCOND PROB OF TRANSITION", uncondTransProb);
                trace(cycle, complementToState) += uncondTransProb;
                DebugPrintValue("    TRACE SET TO", trace(cycle, complementToState));

                // Set unconditional transition probabilities
                utpCycleCol[complementRowIndex] = cycle;
                utpFromCol[complementRowIndex] = fromStateName;
                utpToCol[complementRowIndex] = complementToStateName;
                utpValCol[complementRowIndex] = uncondTransProb;

                // Set transitional values
                valList = transitionalValueDictionary[getTransitionKey(fromStateName, complementToStateName)];
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
                    tvalCycleCol[tvalRowIndex] = cycle;
                    tvalStateCol[tvalRowIndex] = fromStateName;
                    tvalDestCol[tvalRowIndex] = complementToStateName;
                    tvalValCol[tvalRowIndex] = uncondTransProb * valValue;
                    tvalRowIndex++;
                }

                transitionErrors(complementRowIndex, 1) = (complementValue > 1) || (complementValue < 0);
                transitionErrors(complementRowIndex, 3) = std::isnan(complementValue);
            } else {
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
        Named("uncondtransprod") = utpResults,
        Named("transitions") = transitions,
        Named("errors") = transitionErrors,
        Named("transitionalValues") = transitionalValueResults
    );
}