# Formal Specification and Decisions

## Formulas

Using s-expressions (lists) to represent formulas is pretty natural. Every formula is a list and the first element of the formula determines what kind of formula it is. The second part of that means that formulas are written in prefix notation. For anyone who hasn't used prefix notation before, it may seem a little foreign, but it has some advantages (like making the first two sentences of this paragraph true). It's easy to represent conjunctions and disjunctions as lists of formulas (without putting anything in between them). This makes conditionals with large conjunctions as antecedents easier to write and read. It also makes breaking formulas onto separate lines a little easier to read. 

The language is also higher order (since philosophers use those sorts of things a lot, e.g. "If something is good then it ought to be done" is probably best represented with quantifiers over propositions). Additionally, the arguments to a predicate can be anything, a term, a variable, a predicate, and even another formula. So the formulation of what a formula is has to be stratified a little. But it can be done recursively

1. A constant is any string that does not begin with "!" or "?". 

2. A predicate is a string that does begin with "!". 

3. A variable is a string that begins with "?".

4. A _simple_ atomic formula is a either a constant or a list that begins with a predicate and is followed by any number of predicates, constants, or variables. 

5. A conjunction is a list starting with "and" and followed by formulas.

6. A disjunction is a list starting with "or" and followed by formulas.

7. A conditional is a list starting with "implies" followed by exactly two formulas.

8. A negation is a list starting with "not" followed by exactly one formula.

9. An existential is a list starting with "exists" followed by any number of variables followed by a single formula.

10. A universal is a list starting with "forall" followed by any number of variables followed by a single formula.

11. An atomic formula is either a simple atomic formula or a predicate followed by any number of predicates, constants, variables, or formulas. 

12. bottom is a formula

13. Those are all the formulas


On this specification familiar(-ish) things like
```
(implies  p p)
``` 
are formulas. But less familiar things 

```
(forall ?x ?y (implies (!said ?x ?y) ?y))
```
are also formulas.

## Consequence Relation

If you went through the tutorial you will have noticed that the consequence relation of the language does not include the principle `(forall ?p (implies (not (not ?p)) ?p))`. So the consequence relation is constructive. The reason for this is to maintain as much balance between premise operations and goal operations as possible. For every connective there is a rule for what to do with it as a premise and a rule for what to do with it as a conclusion. (I named negation elimination 'Bottom Introduction' since that seems clearer to me.) Adding a button for double negation elimination felt unbalanced for this proof system. As a result, the logic is constructive.

In addition to premise and goal operations for each connective there are two other operations: 

1. Direct proof - This closes a subproof when the goal is also one of the premises. 

2. Assert - This allows a user to prove anything they like in the middle of a proof. It is necessary in the following example: 

``` 
1. p 
2. q 
3. (implies (and p q) r)
SHOW: r
```
In order to use modus ponens on the conditional in premise 3, the formula `(and p q)` must be a premise. It is not a premise, but it can be proved from premises. Clicking 'Assert' and entering `(and p q)` allows the user to prove that formula as a subproof. When that subproof closes the formula is available as a premise. 

I haven't proved whether this consequence relation is consistent. If it weren't for the higher order aspect of quantification, it would be pretty clear that it is. If you find a proof of bottom, it would be awesome to know about it. You can email me at andrew[dot]p[dot]parisi[at]gmail.com

## An Aside On Higher Order Quantification

Some philosophers might think that there are ontological issues using a higher order language. I don't think that's true and have argued for it (see my paper <a href=https://doi.org/10.1007/s11229-018-1725-8>Atomic Ontology</a> which came out in <a href=https://www.springer.com/journal/11229>Synthese</a> back in 2018. As it turns out higher order languages are really useful for formulating philosophical notions, e.g. one can simply write `(forall ?p (implies (!true ?p) ?p))` without worrying about quotation or any other way of associating propositions with objects. Similarly, it is straightforward to write down principles about belief and action (which may or may not be true): 
```
(forall ?x ?p 
 (implies 
  (!believes ?x (!good ?p))
  (!ought (!seesToItThat ?x ?p))))
```
which says something like "If a person believes a proposition to be good then they ought to see to it that that proposition holds. 

