# dream-inertia
Inertia protocol for OCaml Dream 

TODO : 
- [x] shared data
- [x] location method added 
     - Adding the X-Inertia-Location 
- [x] Merged props
     - MergeProps is a property, which items will be merged instead of overwrite.
     - [x] deepMergeProps is also a thing... i need to turn the boolean into a ADT
- [x] Add always props
- [x] add optional props
- [x] Error validation on backend side - Nothing to do
- [x] CSRF token
- [x] History encryption
    - page objec : 
        - EncryptHistory bool
        - ClearHistory bool
- [ ] SSR


- what happen when there are defer and prop that collision with there names ?
- maybe the configure functions should only have one that gives all the params.

