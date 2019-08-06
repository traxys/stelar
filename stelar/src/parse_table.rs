use crate::grammar::{Rule, Symbol};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

///! Action to be taken on the action table according to the lookahead and the state
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Action {
    ///! `Shift(i)` to state `i` by pushing the read symbol on the parse stack
    Shift(usize),
    ///! `Reduce(i)` apply rule `i` on the stack and use the goto table to check where to go next
    Reduce(usize),
    ///! End of parsing
    Done,
}

///! A Parse table being the goto table and the action table
///! The parse table is expensive to create (i'd guess something like `n^2` in the number of
///! symbols in all rules). you can save this table using serde for repeated uses.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ParseTable<T, NT>
where
    T: PartialEq + Eq + Hash,
    NT: PartialEq + Eq + Hash,
{
    pub goto_table: HashMap<(usize, NT), usize>,
    pub action_table: HashMap<(usize, Option<T>), Action>,
    pub rules: Vec<Rule<T, NT>>,
    ///! Terminals possible in each state
    pub expected_in_state: HashMap<usize, HashSet<T>>,
    ///! Number of total states
    pub state_count: usize,
}

#[derive(Debug)]
pub enum ParseTableError {
    InvalidGrammar(GrammarError),
    ActionConflict(ActionTableError),
}

macro_rules! set {
    ( $( $x:expr ),* ) => {  // Match zero or more comma delimited items
        {
            #[allow(unused_mut)]
            let mut temp_set = HashSet::new();  // Create a mutable HashSet
            $(
                temp_set.insert($x); // Insert each item matched into the HashSet
            )*
            temp_set // Return the populated HashSet
        }
    };
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
struct DotRule<T, NT> {
    pub index: usize,
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
            index: rule.index,
        }
    }
    fn is_at_end(&self) -> bool {
        self.after.is_empty()
    }
}

type RuleList<T, NT> = HashMap<NT, Vec<(usize, Vec<Symbol<T, NT>>)>>;

fn fold_rules<T, NT>(rules: Vec<Rule<T, NT>>) -> RuleList<T, NT>
where
    NT: Hash + PartialEq + Eq,
{
    let mut folded = HashMap::new();
    for rule in rules {
        folded
            .entry(rule.lhs)
            .or_insert_with(Vec::new)
            .push((rule.index, rule.rhs));
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
        for (rule_index, rule) in rules.get(&s).unwrap() {
            closed.insert(DotRule::new(Rule {
                lhs: s.clone(),
                rhs: rule.clone(),
                index: *rule_index,
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
fn separate_symbols<T, NT>(rules: &[Rule<T, NT>]) -> (HashSet<T>, HashSet<NT>)
where
    T: PartialEq + Eq + Clone + Hash,
    NT: PartialEq + Eq + Clone + Hash,
{
    let mut terminals = HashSet::new();
    let mut non_terminals = HashSet::new();
    for rule in rules {
        non_terminals.insert(rule.lhs.clone());
        for symbol in &rule.rhs {
            match symbol {
                Symbol::NonTerminal(nt) => non_terminals.insert(nt.clone()),
                Symbol::Terminal(t) => terminals.insert(t.clone()),
            };
        }
    }
    (terminals, non_terminals)
}

fn is_nullable<T, NT>(non_terminal: &NT, rules: &RuleList<T, NT>) -> bool
where
    NT: PartialEq + Eq + Hash,
{
    match rules.get(non_terminal) {
        Some(r) => {
            for (_, rule) in r {
                if rule.is_empty() {
                    return true;
                }
            }
            false
        }
        _ => false,
    }
}

type FollowSets<T, NT> = HashMap<NT, HashSet<Option<T>>>;
fn follow_sets<T, NT>(
    first_sets: &FirstSets<T, NT>,
    rules: &[Rule<T, NT>],
    start_symbol: NT,
) -> FollowSets<T, NT>
where
    T: PartialEq + Eq + Clone + Hash,
    NT: PartialEq + Eq + Clone + Hash,
{
    let mut sets = HashMap::new();
    sets.insert(start_symbol, set![None]);
    for rule in rules {
        for (index, symbol) in rule.rhs.iter().enumerate() {
            if index + 1 != rule.rhs.len() {
                if let Symbol::NonTerminal(nt) = symbol {
                    // Add First(b) to Follow(B) for A -> aBb (b is a String)
                    let mut first_nt = first_of_list(&rule.rhs[index + 1..], first_sets);
                    first_nt.remove(&None);
                    sets.entry(nt.clone())
                        .or_insert_with(HashSet::new)
                        .extend(first_nt)
                }
            }
        }
    }
    loop {
        let mut added_to_set = false;
        for rule in rules {
            for (index, symbol) in rule.rhs.iter().enumerate() {
                if let Symbol::NonTerminal(nt) = symbol {
                    if index + 1 == rule.rhs.len()
                        || first_of_list(&rule.rhs[index + 1..], first_sets).contains(&None)
                    {
                        let follow_other =
                            sets.get(&rule.lhs).cloned().unwrap_or_else(HashSet::new);
                        let set_to_add = sets.entry(nt.clone()).or_insert_with(HashSet::new);
                        for item in follow_other.iter() {
                            added_to_set = added_to_set || set_to_add.insert(item.clone());
                        }
                    }
                }
            }
        }
        if !added_to_set {
            break;
        }
    }
    sets
}

type FirstSets<T, NT> = HashMap<Symbol<T, NT>, HashSet<Option<T>>>;

fn first_sets<T, NT>(
    terminals: &HashSet<T>,
    non_terminals: &HashSet<NT>,
    rules: &[Rule<T, NT>],
    rule_list: &RuleList<T, NT>,
) -> FirstSets<T, NT>
where
    T: Hash + Clone + PartialEq + Eq,
    NT: Hash + Clone + PartialEq + Eq,
{
    let mut sets = HashMap::new();
    for terminal in terminals {
        sets.insert(
            Symbol::Terminal(terminal.clone()),
            set![Some(terminal.clone())],
        );
    }
    for non_terminal in non_terminals {
        sets.insert(
            Symbol::NonTerminal(non_terminal.clone()),
            if is_nullable(non_terminal, rule_list) {
                set![None]
            } else {
                set![]
            },
        );
    }
    loop {
        let mut added_to_set = false;
        for rule in rules {
            let first_of_many = first_of_list(&rule.rhs, &sets);
            let set_to_add = sets
                .get_mut(&Symbol::NonTerminal(rule.lhs.clone()))
                .unwrap();
            for item in first_of_many {
                added_to_set = added_to_set || set_to_add.insert(item);
            }
        }
        if !added_to_set {
            break;
        }
    }
    sets
}

// list must be non empty !
fn first_of_list<T, NT>(
    list: &[Symbol<T, NT>],
    sets: &HashMap<Symbol<T, NT>, HashSet<Option<T>>>,
) -> HashSet<Option<T>>
where
    T: Hash + Clone + PartialEq + Eq,
    NT: Hash + Clone + PartialEq + Eq,
{
    let mut result = HashSet::new();
    let mut add_eps = true;
    for item in list {
        add_eps = add_eps && sets.get(&item).unwrap().contains(&None);
    }
    result.extend(sets.get(&list[0]).unwrap().clone());
    if sets.get(&list[0]).unwrap().contains(&None) && list.len() > 1 {
        result.extend(first_of_list(&list[1..], sets));
    }
    result.remove(&None);
    if add_eps {
        result.insert(None);
    }
    result
}

type ItemSets<T, NT> = HashSet<DotRule<T, NT>>;

#[derive(Debug)]
pub enum ActionTableError {
    ShiftReduceConflict,
    ReduceReduceConflict,
}

fn generate_goto_table<T, NT>(
    goto_sets: &Vec<ItemSets<T, NT>>,
    non_terminals: &HashSet<NT>,
    rules: &RuleList<T, NT>,
) -> HashMap<(usize, NT), usize>
where
    NT: PartialEq + Eq + Clone + Hash,
    T: PartialEq + Eq + Clone + Hash,
{
    let mut goto_table = HashMap::new();
    for (set_index, set) in goto_sets.iter().enumerate() {
        for nt in non_terminals {
            let goto_s = goto(set, Symbol::NonTerminal(nt.clone()), rules);
            if !goto_s.is_empty() {
                let goto_index = goto_sets.iter().position(|s| *s == goto_s).unwrap();
                let trans = (set_index, nt.clone());

                goto_table.insert(trans, goto_index);
            }
        }
    }
    goto_table
}

fn generate_action_table<T, NT>(
    goto_sets: &Vec<ItemSets<T, NT>>,
    follow_sets: &FollowSets<T, NT>,
    rules: &RuleList<T, NT>,
    terminals: &HashSet<T>,
    statrt_rule_index: usize,
) -> Result<HashMap<(usize, Option<T>), Action>, ActionTableError>
where
    T: Hash + Clone + PartialEq + Eq,
    NT: Hash + Clone + PartialEq + Eq,
{
    // TODO: Do reduce
    let mut table = HashMap::new();
    for (set_index, set) in goto_sets.iter().enumerate() {
        for terminal in terminals {
            let goto_s = goto(set, Symbol::Terminal(terminal.clone()), rules);
            if !goto_s.is_empty() {
                let trans = (set_index, Some(terminal.clone()));
                if let Some(Action::Reduce(_)) = table.get(&trans) {
                    return Err(ActionTableError::ShiftReduceConflict);
                }

                let goto_index = goto_sets.iter().position(|s| *s == goto_s).unwrap();
                table.insert(trans, Action::Shift(goto_index));
            }
        }
        for rule in set {
            if rule.is_at_end() {
                if let Some(follow) = follow_sets.get(&rule.lhs) {
                    for terminal in follow {
                        let transition = (set_index, terminal.clone());
                        if let Some(action) = table.get(&transition) {
                            match action {
                                Action::Reduce(i) if *i != rule.index => {
                                    return Err(ActionTableError::ReduceReduceConflict)
                                }
                                Action::Shift(_) => {
                                    return Err(ActionTableError::ShiftReduceConflict)
                                }
                                _ => (),
                            }
                        }
                        table.insert(transition, Action::Reduce(rule.index));
                    }
                }
            }
        }
    }
    for (set_index, set) in goto_sets.iter().enumerate() {
        for rule in set {
            if rule.index == statrt_rule_index && rule.is_at_end() {
                table.insert((set_index, None), Action::Done);
            }
        }
    }
    Ok(table)
}

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

impl From<ActionTableError> for ParseTableError {
    fn from(err: ActionTableError) -> ParseTableError {
        ParseTableError::ActionConflict(err)
    }
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
        let (terminals, non_terminals) = separate_symbols(&rules);

        let first = first_sets(&terminals, &non_terminals, &rules, &folded);
        let follow = follow_sets(&first, &rules, start_rule.lhs.clone());

        let start_index = start_rule.index;
        let sets = generate_sets(start_rule, &folded, &symbols);
        let state_count = sets.len();
        let mut parse_table = ParseTable {
            expected_in_state: HashMap::new(),
            state_count,
            rules,
            action_table: generate_action_table(&sets, &follow, &folded, &terminals, start_index)?,
            goto_table: generate_goto_table(&sets, &non_terminals, &folded),
        };
        for ((state_index, terminal), _) in &parse_table.action_table {
            if let Some(terminal) = terminal {
                parse_table
                    .expected_in_state
                    .entry(*state_index)
                    .or_insert_with(HashSet::new)
                    .insert(terminal.clone());
            }
        }
        Ok(parse_table)
    }
}

#[cfg(feature = "print_table")]
impl<T, NT> ParseTable<T, NT>
where
    T: Hash + std::fmt::Debug + PartialEq + Eq + Clone,
    NT: Hash + std::fmt::Debug + PartialEq + Eq + Clone,
{
    pub fn print_tables(&self) {
        use prettytable::{Attr, Cell, Row, Table};

        let mut terminals = set![None];
        for ((_, term), _) in &self.action_table {
            terminals.insert(term.clone());
        }
        let mut non_terminals = HashSet::new();
        for ((_, nt), _) in &self.goto_table {
            non_terminals.insert(nt.clone());
        }
        let mut action_table = Table::new();
        let mut goto_table = Table::new();
        let (mut terminals, non_terminals): (Vec<_>, Vec<_>) = (
            terminals.into_iter().collect(),
            non_terminals.into_iter().collect(),
        );
        let mut action_header = vec![Cell::new("Action")
            .with_style(Attr::Bold)
            .with_style(Attr::Italic(true))];
        action_header.extend(terminals.iter().map(|t| match t {
            Some(t) => Cell::new(&format!("{:?}", t)).with_style(Attr::Bold),
            None => Cell::new("<EOF>").with_style(Attr::Bold),
        }));
        action_table.add_row(Row::new(action_header));
        let mut goto_header = vec![Cell::new("Goto")
            .with_style(Attr::Bold)
            .with_style(Attr::Italic(true))];
        goto_header.extend(
            non_terminals
                .iter()
                .map(|t| Cell::new(&format!("{:?}", t)).with_style(Attr::Bold)),
        );
        goto_table.add_row(Row::new(goto_header));

        for set in 0..self.state_count {
            let mut action_row = vec![Cell::new(&format!("{}", set))];
            for terminal in &terminals {
                action_row.push(match self.action_table.get(&(set, terminal.clone())) {
                    None => Cell::new(""),
                    Some(Action::Done) => Cell::new("accept"),
                    Some(Action::Shift(i)) => Cell::new(&format!("s{}", i)),
                    Some(Action::Reduce(i)) => Cell::new(&format!("r{}", i)),
                })
            }
            action_table.add_row(Row::new(action_row));
            let mut goto_row = vec![Cell::new(&format!("{}", set))];
            for non_terminal in &non_terminals {
                goto_row.push(match self.goto_table.get(&(set, non_terminal.clone())) {
                    Some(i) => Cell::new(&format!("{}", i)),
                    None => Cell::new(""),
                })
            }
            goto_table.add_row(Row::new(goto_row));
        }
        action_table.printstd();
        goto_table.printstd();
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
    use super::{
        closure, first_sets, fold_rules, follow_sets, generate_sets, get_all_symbols, goto,
        sanity_check, separate_symbols,
    };
    use super::{Action, ParseTable};
    use super::{DotRule, GrammarError};
    use crate::grammar::{Rule, Symbol};
    use lazy_static::lazy_static;
    use prettytable::{Attr, Cell, Row, Table};
    use std::collections::{HashMap, HashSet};

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
    fn generate_parse_table() {
        let mut extended_grammar = vec![Rule {
            index: 0,
            lhs: NT::Ext,
            rhs: vec![Symbol::NonTerminal(NT::E)],
        }];
        extended_grammar.append(&mut TEST_GRAMMAR.clone());
        let start_rule = extended_grammar[0].clone();
        let (terminals, non_terminals) = separate_symbols(&extended_grammar);
        let parse_table = ParseTable::new(extended_grammar, start_rule).unwrap();

        let mut action_table = Table::new();
        let mut goto_table = Table::new();
        let (mut terminals, non_terminals): (Vec<_>, Vec<_>) = (
            terminals.into_iter().map(Option::Some).collect(),
            non_terminals.into_iter().collect(),
        );
        terminals.push(None);
        let mut action_header = vec![Cell::new("Action")
            .with_style(Attr::Bold)
            .with_style(Attr::Italic(true))];
        action_header.extend(terminals.iter().map(|t| match t {
            Some(t) => Cell::new(&format!("{:?}", t)).with_style(Attr::Bold),
            None => Cell::new("<EOF>").with_style(Attr::Bold),
        }));
        action_table.add_row(Row::new(action_header));
        let mut goto_header = vec![Cell::new("Goto")
            .with_style(Attr::Bold)
            .with_style(Attr::Italic(true))];
        goto_header.extend(non_terminals.iter().map(|t| Cell::new(&format!("{:?}", t))));
        goto_table.add_row(Row::new(goto_header));

        for set in 0..=11 {
            let mut action_row = vec![Cell::new(&format!("{}", set))];
            for terminal in &terminals {
                action_row.push(
                    match parse_table.action_table.get(&(set, terminal.clone())) {
                        None => Cell::new(""),
                        Some(Action::Done) => Cell::new("accept"),
                        Some(Action::Shift(i)) => Cell::new(&format!("s{}", i)),
                        Some(Action::Reduce(i)) => Cell::new(&format!("r{}", i)),
                    },
                )
            }
            action_table.add_row(Row::new(action_row));
            let mut goto_row = vec![Cell::new(&format!("{}", set))];
            for non_terminal in &non_terminals {
                goto_row.push(
                    match parse_table.goto_table.get(&(set, non_terminal.clone())) {
                        Some(i) => Cell::new(&format!("{}", i)),
                        None => Cell::new(""),
                    },
                )
            }
            goto_table.add_row(Row::new(goto_row));
        }
        action_table.printstd();
        goto_table.printstd();
    }

    #[test]
    fn invalid_grammar() {
        let grammar = vec![Rule {
            index: 0,
            lhs: NT::E,
            rhs: vec![Symbol::NonTerminal(NT::F), Symbol::Terminal(T::Id)],
        }];
        assert_eq!(Some(GrammarError::NoDeriveRule), sanity_check(&grammar))
    }

    #[test]
    fn valid_grammar() {
        assert_eq!(None, sanity_check(&TEST_GRAMMAR))
    }

    #[test]
    fn goto_test() {
        let folded = fold_rules(TEST_GRAMMAR.clone());
        let mut set = HashSet::new();
        let mut b = DotRule::new(TEST_GRAMMAR[3].clone());
        set.insert(b.clone());
        b.increment();
        let mut a = DotRule::new(TEST_GRAMMAR[2].clone());
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
        let syms = get_all_symbols(&TEST_GRAMMAR);
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
        let folded = fold_rules(TEST_GRAMMAR.clone());
        assert_eq!(HashSet::new(), closure(HashSet::new(), &folded))
    }

    #[test]
    fn test_generate_sets() {
        let mut extended_grammar = vec![Rule {
            index: 0,
            lhs: NT::Ext,
            rhs: vec![Symbol::NonTerminal(NT::E)],
        }];
        extended_grammar.append(&mut TEST_GRAMMAR.clone());
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
        let folded = fold_rules(TEST_GRAMMAR.clone());
        let mut set = HashSet::new();
        let mut item = DotRule::new(Rule {
            index: 5,
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
        let mut a = DotRule::new(TEST_GRAMMAR[4].clone());
        a.increment();
        result.insert(a);
        result.insert(DotRule::new(TEST_GRAMMAR[0].clone()));
        result.insert(DotRule::new(TEST_GRAMMAR[1].clone()));
        result.insert(DotRule::new(TEST_GRAMMAR[2].clone()));
        result.insert(DotRule::new(TEST_GRAMMAR[3].clone()));
        result.insert(DotRule::new(TEST_GRAMMAR[4].clone()));
        result.insert(DotRule::new(TEST_GRAMMAR[5].clone()));
        assert_eq!(closure(set, &folded), result);
    }

    #[test]
    fn compute_follow() {
        let (terminal, non_terminal) = separate_symbols(&TEST_GRAMMAR);
        let folded = fold_rules(TEST_GRAMMAR.clone());
        let first = first_sets(&terminal, &non_terminal, &TEST_GRAMMAR, &folded);
        let follow = follow_sets(&first, &TEST_GRAMMAR, NT::E);

        let mut correct = HashMap::new();
        correct.insert(NT::E, set![None, Some(T::RParen), Some(T::Plus)]);
        correct.insert(
            NT::T,
            set![None, Some(T::Times), Some(T::RParen), Some(T::Plus)],
        );
        correct.insert(
            NT::F,
            set![None, Some(T::Times), Some(T::RParen), Some(T::Plus)],
        );
        assert_eq!(follow, correct);
    }

    #[test]
    fn compute_first() {
        let mut correct = HashMap::new();
        correct.insert(Symbol::Terminal(T::Id), {
            let mut set = HashSet::new();
            set.insert(Some(T::Id));
            set
        });
        correct.insert(Symbol::Terminal(T::LParen), {
            let mut set = HashSet::new();
            set.insert(Some(T::LParen));
            set
        });
        correct.insert(Symbol::Terminal(T::Plus), {
            let mut set = HashSet::new();
            set.insert(Some(T::Plus));
            set
        });
        correct.insert(Symbol::Terminal(T::RParen), {
            let mut set = HashSet::new();
            set.insert(Some(T::RParen));
            set
        });
        correct.insert(Symbol::Terminal(T::Times), {
            let mut set = HashSet::new();
            set.insert(Some(T::Times));
            set
        });

        correct.insert(Symbol::NonTerminal(NT::E), {
            let mut set = HashSet::new();
            set.insert(Some(T::LParen));
            set.insert(Some(T::Id));
            set
        });
        correct.insert(Symbol::NonTerminal(NT::F), {
            let mut set = HashSet::new();
            set.insert(Some(T::LParen));
            set.insert(Some(T::Id));
            set
        });
        correct.insert(Symbol::NonTerminal(NT::T), {
            let mut set = HashSet::new();
            set.insert(Some(T::LParen));
            set.insert(Some(T::Id));
            set
        });

        let folded = fold_rules(TEST_GRAMMAR.clone());
        let (term, non_term) = separate_symbols(&TEST_GRAMMAR);
        let first = first_sets(&term, &non_term, &TEST_GRAMMAR, &folded);
        assert_eq!(first, correct);
    }

    lazy_static! {
        static ref TEST_GRAMMAR: Vec<Rule<T, NT>> = vec![
            Rule {
                index: 1,
                lhs: NT::E,
                rhs: vec![
                    Symbol::NonTerminal(NT::E),
                    Symbol::Terminal(T::Plus),
                    Symbol::NonTerminal(NT::T),
                ],
            },
            Rule {
                index: 2,
                lhs: NT::E,
                rhs: vec![Symbol::NonTerminal(NT::T)],
            },
            Rule {
                index: 3,
                lhs: NT::T,
                rhs: vec![
                    Symbol::NonTerminal(NT::T),
                    Symbol::Terminal(T::Times),
                    Symbol::NonTerminal(NT::F),
                ],
            },
            Rule {
                index: 4,
                lhs: NT::T,
                rhs: vec![Symbol::NonTerminal(NT::F)],
            },
            Rule {
                index: 5,
                lhs: NT::F,
                rhs: vec![
                    Symbol::Terminal(T::LParen),
                    Symbol::NonTerminal(NT::E),
                    Symbol::Terminal(T::RParen),
                ],
            },
            Rule {
                index: 6,
                lhs: NT::F,
                rhs: vec![Symbol::Terminal(T::Id)],
            },
        ];
    }
}
