# Welcome to λogos

Λogos is a proof assistant. It helps users keep track of the where they are in a proof and what they have left to prove to complete a proof. λogos is good for practice, teaching, but I mostly use it to work through philosophical arguments.

This page is a tutorial on how to use λogos. It starts with a brief explanation of how formulas are represented (with a full explanation). The second section of this page gives a step by step example of a proof that can be followed along in another browser window.

# Sample Proof

<img align="right" style="width: 49%;" src="hatter.jpg">

One of my favorite books is 'Alice in Puzzleland' by Raymond Smullyan. There are plenty of great puzzles there all of which can be represented in a formal system. This tutorial will work through one of them.

Here's a quote that lays out the first puzzle of the book

<blockquote align="left" style="width: 49%;">
Well, recovering the sugar turned out to be a relatively simple affair. The sugar was found in the house of the Duchess, and as events proved, it was stolen by either the Duchess or the Cook, but not both. They made the following statements at the trial:

DUCHESS: The cook did not steal the sugar.

COOK: The Duchess stole the sugar.

The one who stole the sugar was lying. (It is not given whether the other one lied or told the truth.)
</blockquote>

## Writing the Problem Down

Each portion of the puzzle can be represented in the language described above. Each claim is translated in turn.

### There are two Thieves

> it was stolen by either the Duchess or the Cook

is represented as

```
(or (!steal duchess) (!steal cook))
```

### What the Duchess Said

The claim
> DUCHESS: The cook did not steal the sugar

is represented as

```
(!said duchess (not (!steal cook)))
```

### Lying Thief

The claim
> The one who stole the sugar was lying.

is

```
(forall ?x
  (implies
   (!steal ?x)
   (forall ?p
    (implies (!said ?x ?p) (not ?p)))))
```

It follows from those premises that the cook is the thief with one additional principle:

```
(forall ?x (implies (not (not (!steal ?x))) (!steal ?x)))
```

The reason for requiring this as an explicit premise is discussed in the Consequence Relation section below.

### Full Formula

Putting everyting together we are trying to prove this formula:

```lisp
(implies
 (and
  (forall ?x (implies (not (not (!steal ?x))) (!steal ?x)))
  (or (!steal duchess) (!steal cook))
  (!said duchess (not (!steal cook)))
  (forall ?x
   (implies
    (!steal ?x)
    (forall ?p
     (implies (!said ?x ?p) (not ?p))))))
 (!steal cook))
 ```

## Proof

To start a proof go to the main page and enter a name for the theorem. For simplicity, let's call this theorem 'guilty-cook', but you can name it anything you'd like. Copy the above formula into the box labeled 'Formula' and click the 'Start Proof' button.  

This takes you to a proof page with your formula on the left and buttons for performing proof operations on the right. For the most part there are two buttons for each connective, one for doing something when it's the main connective of the formula to be proved and one for when it's the main connective of a formula in your premise set.

For now, the only thing in the proof is the goal, which is the formula you entered on the previous page. Since the main connective of that formula is `implies`, the 'Conditional Proof' button is the one to press.

<ol>
 <li> Press the 'Conditional Proof' button. </li>

 <li> Now the proof has a premise and a goal. The goal is `(!steal cook)` and the premise is a big conjunction. Break down that conjunction by clicking on 'Conjunction Elimination', selecting the check box next to the premise and clicking 'Submit Command'. </li>

  <li> The proof should have gotten four more premises to work with. Let's focus on the disjunction: `(or (!steal duchess) (!steal cook))`. Click 'Disjunction Elimination', select that premise, and hit 'Submit Command'. 
  
  Disjunction Elimination breaks down the disjunction and creates two new proofs. The first one, with `(!steal duchess)` as a premise should be visible now. The second one has `(!steal cook)` as a premise. Both have the same goal, which is the same as the goal before clicking on 'Disjunction Elimination'. The idea is this. Suppose you know that either `A` is true or `B` is true and you are trying to show `C`. If you can show that `C` is true when `A` is _and_ you can show that `C` is true when `B` is, that's enough to show that `C` is true. </li>

 <li> Since we've hypothesized that the duchess is the thief `(!steal duchess)`, we can use the premise about thieving liars
```
(forall ?x
 (implies
  (!steal ?x)
  (forall ?p
   (implies (!said ?x ?p) (not ?p)))))
```
 <ol>
  <li> First we'll need to remove the universal. To do this click the 'Universal Elimination' button. On the pop-up select that premise, enter 'duchess' in the 'Enter list of values for variables' box, and click 'Submit Command'. </li>

  <li> You should now have a new premise 
```
(implies (!steal duchess) (forall ?p (implies (!said duchess ?p) (not ?p))))
```

This is a conditional and so you can use the 'Conditional Elimination' buttom to operate on it. Importantly, to operate on a conditional you also have to have the first formula in the conditional, in this case `(!steal duchess)` as a premise. Luckily, it's there. So click on 'Conditional Elimination' and select *two* check-boxes, the one next to the new premise and the one next to `(!steal duchess)`. Finally, click 'Submit Command'. </li>

  <li> Now you should have another premise 
```
(forall ?p (implies (!said duchess ?p) (not ?p)))
```
This again is a universal. Click 'Universal Elimination' and select that premise. This time we are going to use a formula as the value for a variable. In particular, we want to substitute the thing that the duchess said '(not (!steal cook))'.

Clicking the 'Submit Command' button adds a new conditional premise.
</li>

<li> The new premise
```
(implies (!said duchess (not (!steal cook))) (not (not (!steal cook))))
```
can be used with the premise about what the duchess said `(!said duchess (not (!steal cook)))` using the 'Conditional Elimination' button. Click that button and select the new premise and the one about what the duchess said. 
</li>
 </ol>

<li>
After all those steps there is a new premise about the cook `(not (not (!steal cook)))`. This can be used with the premise about double negations and stealing
```
(forall ?x (implies (not (not (!steal ?x))) (!steal ?x)))
```
Click on 'Universal Elimination' and use 'cook' as the value. Use the 'Conditional Elimination' button on the result and the premise generated in the step above.
</li>

<li>
At this point you should have a premise that says the cook is guilty `(!steal cook)`. Since that's what we're trying to show, click 'Direct Proof' to end this subproof. 
</li>

<li> 
Since in step 3 we used disjunction elimination. We have to prove that the cook is guilty again, but this time with the second disjunct as a hypothesis. Fortunately, this case is pretty straightforward since the second disjunct is `(!steal cook)`. So for this case we can just click 'Direct Proof' again. 
</li> 

<li> 
That's it! You should see 'QED' appear on the screen. You can click 'Next Proof' to start another proof. 
</li>
</ol> 

<img style="width: 10%;" src="cook.jpg">

# Quick Introduction to Formulas

The main object of proofs are formulas. A formula is a formal representation of a proposition (somethng that is capable  of being true or false). Here are some examples of formulas that λogos understands:

1. `(implies p p)`
2. `(not (and (not p) p))`
3. `(not (not (forall ?x (or (!F ?x) (not (!F ?x)))))) `

Whitespace and newlines don't matter, so

```lisp
(implies
 (and
  (forall ?x (implies (!F ?x) (!G ?x)))
  (exists ?y (!F ?y)))
(exists ?z (!G ?z)))
```
is also a well-formed formula

Prefix (a.k.a Polish) notation is awesome. If you're used to infix notation give prefix notation a minute or two to get used to. Every formula starts with a connective ('and', 'or', 'not', or 'implies), quantifier ('forall' or 'exists'), or predicate (string starting with '!'). Connectives are followed by other formulas, quantifiers followed by variables (strings starting with '?') and then a formula, and predicate are followed by almost anything (strings, variables, or formulas).

For a more in depth discussion of formulas and the consequence relation visit <a href="#/formulas">Formulas</a>
