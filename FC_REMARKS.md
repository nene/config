# Async

- Often Observables are converted to Promises.
- Lots of raw Promise() syntax, fairly little `async/await`.
- Alarming amount of setTimeout() calls.

# Types

- Lots of type-casts.
- Lack of generics (instead lots of `any` types)
- Bit of a lack of function types.

# Components

- Tend to be pretty big.
- Specifically the template and CSS files can be overwhelming.
- Overriding sub-component styles with `::ng-deep`.

# Tests

- `spyOn()` used on private methods
- Lots of tests are just smoke-tests or placeholders.
- Several tests are very rudimentary, only covering a fraction of functionality.

# Code style

- Lots of interfaces have `I`-prefix, others not.
- Multiple forms of using `@Injectable`.
- Some files use different indentation than 2 spaces.
- Lots of `null` instead of `undefined`.

# Duplication

- `new Subject(null)` followed by filtering out the null.
- `base.service` is pretty similar in multiple projects.
- `ngOnDestroy()` which unsubscribes from all.
- Several uses of `reduce()` for summing up numbers, instead of having a `sum()` function.

# Other

- No shared utils or utils-library
- Lots of forEach() instead of map(), filter() etc.

# Questions

- Why still Angular 9 when 11 is already out
-

# Links

- https://blog.bitsrc.io/the-principles-for-writing-awesome-angular-components-10e45f9ae77e
