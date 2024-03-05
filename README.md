# Crap

Proof of concept argument parser in [roc](https://roc-lang.org) using its
`Decode` builtin. This allows a record to be used as a specification for cli arguments.

## Notes

This is far from a full feature command line parsing library. As the first
line in the description says, this is a proof of concept. There are probably
many builtin limitations to what this way of defining a cli spec can do, like
defining custom short names. That said, I think it is cool to know something
basic is even possible with the system.

## Some idea for someone to work on?

Most similar libraries, like rust's clap, have some way of automatically
generating help docs. Maybe something basic could be done with a custom
encoder?
