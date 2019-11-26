# Interview with *N N* Time slot: 1:30

# About Me:
Hello! My name is Rene Saarsoo and I am working as a software developer at Taxify.
At the moment I am working in the new Food Delivery team.
Creating a mobile app for food couriers which they would use while delivering the food.

# Summary:
Today I have a live coding task for you and I am here to answer your questions when you do this.
After the task we have 5-10 minutes and I can answer your questions regarding life in Taxify and in the Engineering team.

# About coding:
Before I give you the task, I would like to give a small overview on what we expect during the interview:

- It would be nice if you share your thoughts while you are coding.
- We don't check semicolons - what matters is idea correctness.
- If you hear keyboard sound it means I am taking notes
- We don't allow running code before both parties agree that it's finished

# TASK:

Please choose a language and you can start.

```
'(', '{', '[' are called "openers".
')', '}', ']' are called "closers".
Write an efficient function that tells us whether input string's openers
and closers are properly nested.


Examples:
"{ [ ] ( ) }" -> true
"{ [ ( ] ) }" -> false
"{ [ }" -> false
"class Example { public do() { return; } }" -> true
"class Example { public do( { return; } }" -> false
```

Extensions:

```
Add support for a new pair of opener/closer: <, >

Add support for 3 self-closing parentheses: " ' |
```


# Subsequence sum:

```
You are given an array A of non-negative numbers and a target sum S.

Write a function that finds one contiguous sub-sequence of elements who sum up to precisely S.

The return value should be a pair of array indices or null.

Examples:

A = [ 1, 2, 3 ], S = 5. Result = [ 1, 2 ]
```

Extensions:

Now imagine the numbers are arbitrary integers (what if we must allow them to be double, how can we model that?).

Note: Sought after solution would be O(n) or O(n * log n) running time with O(n) or O(n * log n) extra space. O(n) preferred of course, with Hashing. Can also generate a question about Hashing theory / Balanced Trees theory, see how deep a candidate is knowledgeable about these. Solution involves computing prefix sums for the array.


Test cases

```
[2, 3, 1, 4, 2, 9], 7 → (2, 4) because a[2]+a[3]+a[4] = 1 + 4 + 2 = 7.

[1, 0, 2, 4, 5], 0 → (1,1) - potential corner case with 0 appearing

[1, 16, 200], 0 → null - there is no non-empty sub-sequence with sum 0. 

[], 15 → null - empty array; should return null, not crash

[], 0 → null - empty array; should return null, not crash or return (-1,0) or anything else.
```

# Test cases to consider

```
// strings without parentheses
"" -> true
"whatever" -> true
// opening without closing
"(" -> false
// closing without opening
")" -> false
// balanced by count, but incorrectly nested
"( [ ) ]" -> false

// self-closing parens
"''" -> true
"'''" -> false
"'||'" -> true
"'|'|" -> false
```

# Thank the candidate for his time


- Save his work before quitting

