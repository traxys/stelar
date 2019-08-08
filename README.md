# stelar
stelar is an SLR parser

Why stelar ? Because SteLaR.

The generation of the SLR parse table is absolutely not optimized, as such you should generate it once, save it somewhere by serializing it with serde and get it back that way

## Usage

You will need to define two types, `T` and `NT`, both need to be `Hash + Clone + PartialEq + Eq`.

Morever if you want more meaningfull errors you should have `Debug`.

`T` is the token, or terminal type, representing the input values.
`NT` are the grammar construct.

You then have to define rules of the form `(NT, Vec<Symbol<T, NT>>)` and call `create_rules`.

To make the rule definition easier there is a macro `rule_rhs![]`, it works by creating a `Vec` of symbols such that `[..., Foo, ...]` is mapped to `Symbol::Terminal(Foo)` and `[...., (Bar), ....]` is mapped to `Symbol::NonTerminal(Bar)`.

And a grammar `g` is a `Vec` of such rules, such that `g[i].index == i`.

With this grammar you can generate a `ParseTable<T, NT>`.

You will then need an iterator of `ValuedToken<T, V>`, being a token `T` maybe associated with a Value `V` (`Option<V>`).

With this you can create and parse data.

## Example
An example of how evrything is done is provided in `calc_grammar`
