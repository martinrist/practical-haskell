# 'Practical Haskell' Study Notes

## Part I: First Steps

- [x] [Chapter 2 - Declaring the Data Model](chapter02/README.md)
- [x] [Chapter 3 - Increasing Code Reuse](chapter03/README.md)
- [ ] [Chapter 4 - Using Containers & Type Classes](chapter04/README.md)
- [ ] Chapter 5 - Laziness & Infinite Structures

## Part II: Data Mining

- [ ] Chapter 6 - Knowing Your Clients Using Monads
- [ ] Chapter 7 - More Monads: Now for Recommendations
- [ ] Chapter 8 - Working in Several Cores

## Part III: Resource Handling

- [ ] Chapter 9 - Dealing with Files: IO & Conduit
- [ ] Chapter 10 - Building & Parsing Text
- [ ] Chapter 11 - Safe Database Access
- [ ] Chapter 12 - Web Applications

## Part IV: Domain Specific Languages

- [ ] Chapter 13 - Strong Types for Describing Offers
- [ ] Chapter 14 - Interpreting Offers with Attributes

## Part V: Engineering the Store

- [ ] Chapter 15 - Documenting, Testing & Verifying
- [ ] Chapter 16 - Architecting Your Application
- [ ] Chapter 17 - Looking Further


# Creating Chapter Projects

To create a new project for `chapter<n>`:

- Create a new empty project - `stack new chapter<n>`.
- Review the new `stack.yaml` file and make sure it's using a resolver that is
  consistent with the remaining chapters.
- Review the project metadata in `chapter<n>/package.yaml`.
- Remove the `Setup.hs` file.
- Generate the `hie.yaml` file - `gen-hie > hie.yaml`
- Test the new project setup:
    - `stack test`
    - `haskell-language-server-wrapper`
- Create blank `chapter<n>/README.md` file and update link in main `README.md` file.
- Commit files to Git.