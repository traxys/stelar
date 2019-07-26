#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

///! Trait to extract values out of a input storage
pub trait Extract<I>: Sized {
    fn extract(input: &mut I) -> Option<Self>;
}

pub struct TokenStream<I, T>
where
    T: Extract<I>,
{
    input: I,
    _marker: std::marker::PhantomData<T>,
}

impl<T: Extract<I>, I> TokenStream<I, T> {
    pub fn new(input: I) -> TokenStream<I, T> {
        TokenStream {
            input,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<I, T> Iterator for TokenStream<I, T>
where
    T: Extract<I>,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        T::extract(&mut self.input)
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Symbol<T, NT> {
    Terminal(T),
    NonTerminal(NT),
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Action {
    Shift(usize),
    Reduce(usize),
    Done,
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Rule<T, NT> {
    pub lhs: NT,
    pub rhs: Vec<Symbol<T, NT>>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
struct DotRule<T, NT> {
    pub lhs: NT,
    pub before: Vec<Symbol<T, NT>>,
    pub after: Vec<Symbol<T, NT>>,
}

impl<T, NT> DotRule<T, NT> {
    fn increment(&mut self) -> bool {
        if self.after.len() != 0 {
            let temp = self.after.split_off(1);
            self.before.push(self.after.pop().unwrap());
            self.after = temp;
            true
        } else {
            false
        }
    }
    fn after_dot(&self) -> Option<&Symbol<T, NT>> {
        self.after.first()
    }
    fn new(rule: Rule<T, NT>) -> Self {
        DotRule {
            before: Vec::new(),
            after: rule.rhs,
            lhs: rule.lhs,
        }
    }
}

type RuleList<T, NT> = HashMap<NT, Vec<Vec<Symbol<T, NT>>>>;

fn fold_rules<T, NT>(rules: Vec<Rule<T, NT>>) -> RuleList<T, NT>
where
    NT: Hash + PartialEq + Eq,
{
    let mut folded = HashMap::new();
    for rule in rules {
        folded
            .entry(rule.lhs)
            .or_insert_with(Vec::new)
            .push(rule.rhs);
    }
    folded
}

fn close_item<T, NT>(rules: &RuleList<T, NT>, item: &DotRule<T, NT>) -> HashSet<DotRule<T, NT>>
where
    T: PartialEq + Eq + Hash + Clone,
    NT: PartialEq + Eq + Hash + Clone,
{
    let mut closed = HashSet::new();
    if let Some(Symbol::NonTerminal(s)) = item.after_dot() {
        for rule in rules.get(&s).unwrap() {
            closed.insert(DotRule::new(Rule {
                lhs: s.clone(),
                rhs: rule.clone(),
            }));
        }
    }
    closed
}

fn closure<T, NT>(set: HashSet<DotRule<T, NT>>, rules: &RuleList<T, NT>) -> HashSet<DotRule<T, NT>>
where
    T: PartialEq + Eq + Hash + Clone,
    NT: PartialEq + Eq + Hash + Clone,
{
    let mut closed = set;
    let mut to_add = Vec::new();
    loop {
        to_add.clear();
        for item in &closed {
            for new_rule in close_item(rules, item) {
                if !closed.contains(&new_rule) {
                    to_add.push(new_rule)
                }
            }
        }
        if to_add.is_empty() {
            break;
        } else {
            closed.extend(to_add.drain(0..));
        }
    }
    closed
}

fn goto<T, NT>(
    set: &HashSet<DotRule<T, NT>>,
    symbol: Symbol<T, NT>,
    rules: &RuleList<T, NT>,
) -> HashSet<DotRule<T, NT>>
where
    T: PartialEq + Eq + Hash + Clone,
    NT: PartialEq + Eq + Hash + Clone,
{
    let mut to_close = HashSet::new();
    for rule in set {
        if let Some(s) = rule.after.first() {
            if !rule.after.is_empty() && *s == symbol {
                let mut augment_rule = rule.clone();
                augment_rule.increment();
                to_close.insert(augment_rule);
            }
        }
    }
    closure(to_close, rules)
}

fn get_all_symbols<T, NT>(rules: &Vec<Rule<T, NT>>) -> HashSet<Symbol<T, NT>>
where
    T: PartialEq + Eq + Clone + Hash,
    NT: PartialEq + Eq + Clone + Hash,
{
    let mut symbols = HashSet::new();
    for rule in rules {
        symbols.insert(Symbol::NonTerminal(rule.lhs.clone()));
        for symbol in &rule.rhs {
            symbols.insert(symbol.clone());
        }
    }
    symbols
}

type ItemSets<T, NT> = HashSet<DotRule<T, NT>>;

fn generate_sets<T, NT>(
    start_rule: Rule<T, NT>,
    rules: &RuleList<T, NT>,
    symbols: &HashSet<Symbol<T, NT>>,
) -> Vec<ItemSets<T, NT>>
where
    T: PartialEq + Eq + Clone + Hash,
    NT: PartialEq + Eq + Clone + Hash,
{
    let mut sets = vec![];
    let mut initial_rule = HashSet::new();
    initial_rule.insert(DotRule::new(start_rule));
    sets.push(closure(initial_rule, rules));
    loop {
        let mut to_add = Vec::new();
        for set in &sets {
            for sym in symbols {
                let goto_s = goto(&set, sym.clone(), rules);
                if !goto_s.is_empty() && !sets.contains(&goto_s) && !to_add.contains(&goto_s) {
                    to_add.push(goto_s);
                }
            }
        }
        if to_add.is_empty() {
            break;
        }
        sets.append(&mut to_add);
    }
    sets
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ParseTable<T, NT>
where
    T: Clone + PartialEq + Eq + Hash,
    NT: Clone + PartialEq + Eq + Hash,
{
    table: HashMap<(usize, NT), Action>,
    pub rules: Vec<Rule<T, NT>>,
}

#[derive(Debug)]
pub enum ParseTableError {
    InvalidGrammar(GrammarError),
}

impl<T, NT> ParseTable<T, NT>
where
    T: Clone + PartialEq + Eq + Hash,
    NT: Clone + PartialEq + Eq + Hash,
{
    pub fn new(
        rules: Vec<Rule<T, NT>>,
        start_rule: Rule<T, NT>,
    ) -> Result<ParseTable<T, NT>, ParseTableError> {
        if let Some(e) = sanity_check(&rules) {
            return Err(ParseTableError::InvalidGrammar(e));
        };
        let folded = fold_rules(rules.clone());
        let symbols = get_all_symbols(&rules);
        let _sets = generate_sets(start_rule, &folded, &symbols);
        Ok(ParseTable {
            rules,
            table: HashMap::new(),
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum GrammarError {
    NoDeriveRule,
}

fn sanity_check<T, NT: PartialEq + Eq + Hash>(rules: &Vec<Rule<T, NT>>) -> Option<GrammarError> {
    let mut non_terminals = HashSet::new();
    non_terminals.extend(rules.iter().map(|r| &r.lhs));
    if rules.iter().any(|v| {
        v.rhs.iter().any(|s| match s {
            Symbol::NonTerminal(nt) => !non_terminals.contains(nt),
            _ => false,
        })
    }) {
        Some(GrammarError::NoDeriveRule)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::GrammarError;
    use crate::Symbol;
    use crate::{closure, fold_rules, generate_sets, get_all_symbols, goto, sanity_check};
    use crate::{DotRule, Rule};
    use lazy_static::lazy_static;
    use std::collections::HashSet;

    fn add_dot_rule<T, NT>(rule: &mut DotRule<T, NT>, times: usize) {
        for _ in 0..times {
            rule.increment();
        }
    }
    fn dot_rule_in_pos<T, NT>(rule: Rule<T, NT>, pos: usize) -> DotRule<T, NT> {
        let mut dot = DotRule::new(rule);
        add_dot_rule(&mut dot, pos);
        dot
    }

    #[derive(PartialEq, Eq, Hash, Clone, Debug)]
    enum NT {
        Ext,
        E,
        T,
        F,
    }
    #[derive(PartialEq, Eq, Hash, Clone, Debug)]
    enum T {
        LParen,
        RParen,
        Id,
        Plus,
        Times,
    }

    #[test]
    fn invalid_grammar() {
        let grammar = vec![Rule {
            lhs: NT::E,
            rhs: vec![Symbol::NonTerminal(NT::F), Symbol::Terminal(T::Id)],
        }];
        assert_eq!(Some(GrammarError::NoDeriveRule), sanity_check(&grammar))
    }

    #[test]
    fn valid_grammar() {
        assert_eq!(None, sanity_check(&test_grammar))
    }

    #[test]
    fn goto_test() {
        let folded = fold_rules(test_grammar.clone());
        let mut set = HashSet::new();
        let mut b = DotRule::new(test_grammar[3].clone());
        set.insert(b.clone());
        b.increment();
        let mut a = DotRule::new(test_grammar[2].clone());
        a.increment();
        a.increment();
        set.insert(a.clone());
        a.increment();
        let goto_set = goto(&set, Symbol::NonTerminal(NT::F), &folded);
        let mut result_set = HashSet::new();
        result_set.insert(a);
        result_set.insert(b);
        assert_eq!(goto_set, result_set);
    }

    #[test]
    fn get_symbols() {
        let syms = get_all_symbols(&test_grammar);
        let mut expected = HashSet::new();
        expected.insert(Symbol::NonTerminal(NT::E));
        expected.insert(Symbol::NonTerminal(NT::T));
        expected.insert(Symbol::NonTerminal(NT::F));
        expected.insert(Symbol::Terminal(T::Id));
        expected.insert(Symbol::Terminal(T::LParen));
        expected.insert(Symbol::Terminal(T::Plus));
        expected.insert(Symbol::Terminal(T::RParen));
        expected.insert(Symbol::Terminal(T::Times));
        assert_eq!(expected, syms);
    }

    #[test]
    fn empty_closure() {
        let folded = fold_rules(test_grammar.clone());
        assert_eq!(HashSet::new(), closure(HashSet::new(), &folded))
    }

    #[test]
    fn test_generate_sets() {
        let mut extended_grammar = vec![Rule {
            lhs: NT::Ext,
            rhs: vec![Symbol::NonTerminal(NT::E)],
        }];
        extended_grammar.append(&mut test_grammar.clone());
        let folded = fold_rules(extended_grammar.clone());
        let symbols = get_all_symbols(&extended_grammar);
        let sets = generate_sets(extended_grammar[0].clone(), &folded, &symbols);

        let mut expected_sets = Vec::new();
        let mut set = HashSet::new(); // 0
        set.insert(dot_rule_in_pos(extended_grammar[0].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[1].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[2].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[3].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[4].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[5].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[6].clone(), 0));
        expected_sets.push(set);

        let mut set = HashSet::new(); // 1
        set.insert(dot_rule_in_pos(extended_grammar[0].clone(), 1));
        set.insert(dot_rule_in_pos(extended_grammar[1].clone(), 1));
        expected_sets.push(set);

        let mut set = HashSet::new(); // 2
        set.insert(dot_rule_in_pos(extended_grammar[2].clone(), 1));
        set.insert(dot_rule_in_pos(extended_grammar[3].clone(), 1));
        expected_sets.push(set);

        let mut set = HashSet::new(); // 3
        set.insert(dot_rule_in_pos(extended_grammar[4].clone(), 1));
        expected_sets.push(set);

        let mut set = HashSet::new(); // 4
        set.insert(dot_rule_in_pos(extended_grammar[5].clone(), 1));
        set.insert(dot_rule_in_pos(extended_grammar[1].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[2].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[3].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[4].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[5].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[6].clone(), 0));
        expected_sets.push(set);

        let mut set = HashSet::new(); // 5
        set.insert(dot_rule_in_pos(extended_grammar[6].clone(), 1));
        expected_sets.push(set);

        let mut set = HashSet::new(); // 6
        set.insert(dot_rule_in_pos(extended_grammar[1].clone(), 2));
        set.insert(dot_rule_in_pos(extended_grammar[3].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[4].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[5].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[6].clone(), 0));
        expected_sets.push(set);

        let mut set = HashSet::new(); // 7
        set.insert(dot_rule_in_pos(extended_grammar[3].clone(), 2));
        set.insert(dot_rule_in_pos(extended_grammar[5].clone(), 0));
        set.insert(dot_rule_in_pos(extended_grammar[6].clone(), 0));
        expected_sets.push(set);

        let mut set = HashSet::new(); // 8
        set.insert(dot_rule_in_pos(extended_grammar[5].clone(), 2));
        set.insert(dot_rule_in_pos(extended_grammar[1].clone(), 1));
        expected_sets.push(set);

        let mut set = HashSet::new(); // 9
        set.insert(dot_rule_in_pos(extended_grammar[1].clone(), 3));
        set.insert(dot_rule_in_pos(extended_grammar[3].clone(), 1));
        expected_sets.push(set);

        let mut set = HashSet::new(); // 10
        set.insert(dot_rule_in_pos(extended_grammar[3].clone(), 3));
        expected_sets.push(set);

        let mut set = HashSet::new(); // 11
        set.insert(dot_rule_in_pos(extended_grammar[5].clone(), 3));
        expected_sets.push(set);

        if expected_sets.len() != sets.len() {
            eprintln!("Expected {:?}", expected_sets);
            eprintln!("------------------------------------");
            eprintln!("Got {:?}", sets);
            panic!();
        }
        for set in expected_sets {
            if !sets.contains(&set) {
                panic!("Set {:?} is not present", set);
            }
        }
    }

    #[test]
    fn simple_closure() {
        let folded = fold_rules(test_grammar.clone());
        let mut set = HashSet::new();
        let mut item = DotRule::new(Rule {
            lhs: NT::F,
            rhs: vec![
                Symbol::Terminal(T::LParen),
                Symbol::NonTerminal(NT::E),
                Symbol::Terminal(T::RParen),
            ],
        });
        item.increment();
        set.insert(item);
        let mut result = HashSet::new();
        let mut a = DotRule::new(test_grammar[4].clone());
        a.increment();
        result.insert(a);
        result.insert(DotRule::new(test_grammar[0].clone()));
        result.insert(DotRule::new(test_grammar[1].clone()));
        result.insert(DotRule::new(test_grammar[2].clone()));
        result.insert(DotRule::new(test_grammar[3].clone()));
        result.insert(DotRule::new(test_grammar[4].clone()));
        result.insert(DotRule::new(test_grammar[5].clone()));
        assert_eq!(closure(set, &folded), result);
    }

    lazy_static! {
        static ref test_grammar: Vec<Rule<T, NT>> = vec![
            Rule {
                lhs: NT::E,
                rhs: vec![
                    Symbol::NonTerminal(NT::E),
                    Symbol::Terminal(T::Plus),
                    Symbol::NonTerminal(NT::T),
                ],
            },
            Rule {
                lhs: NT::E,
                rhs: vec![Symbol::NonTerminal(NT::T)],
            },
            Rule {
                lhs: NT::T,
                rhs: vec![
                    Symbol::NonTerminal(NT::T),
                    Symbol::Terminal(T::Times),
                    Symbol::NonTerminal(NT::F),
                ],
            },
            Rule {
                lhs: NT::T,
                rhs: vec![Symbol::NonTerminal(NT::F)],
            },
            Rule {
                lhs: NT::F,
                rhs: vec![
                    Symbol::Terminal(T::LParen),
                    Symbol::NonTerminal(NT::E),
                    Symbol::Terminal(T::RParen),
                ],
            },
            Rule {
                lhs: NT::F,
                rhs: vec![Symbol::Terminal(T::Id)],
            },
        ];
    }
}
