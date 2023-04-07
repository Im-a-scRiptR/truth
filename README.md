# truth

Have you ever wanted to check your work on long, tedious, and demanding truth tables?
This short script of functions, truth, allows you to plug in almost any plain English,
ALL CAPS, statement(s) of propositional logic and compose a corresponding truth
table.

Rules for Statements
- All statements containing "IMPLIES" must be fully enclosed in parentheses
- The statements containing "IFF" must be fully enclosed in parentheses
- Expressions cannot be duplicated: c("(NOT P OR Q)", "NOT P OR Q") throws error
- You can reverse it however: c("NOT Q OR P", "NOT P OR Q")
- The count(vars) can be as many as you please

The below example is from Vellman's "How to Prove It" (3rd edition Section 1.2 
Exercise 7(b))

```
vars <- c("P","Q","R","M")
prems <- c(
  "(P OR Q)",
  "(R OR M)",
  "(NOT (P AND R))",
  "(NOT (Q AND M))"
)
ttbl <- vars %>% make_truth_tbl(prems)
ttbl
ttbl %>% is_valid_ttbl()

```
One should expect the truth table for this to be invalid with 3 rows contributing
to it being invalid.

One should also try the below to assure yourself the conditional logic is working:
```
vars <- c("P","Q","R","M")
prems <- c(
  "(P OR Q)",
  "(R OR M)",
  "(NOT (P AND R))",
  "(NOT (Q AND M))",
  "(P IMPLIES Q)",
  "((P OR Q) IFF (Q AND P))"
)
ttbl <- vars %>% make_truth_tbl(prems)
ttbl
ttbl %>% is_valid_ttbl()

```
