use std::borrow::Cow;
use std::cell::{RefCell, RefMut};
use std::collections::{BTreeMap, VecDeque};
use std::fmt;
use std::iter::FromIterator;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Name {
    Long(String),
    Short(char),
}

impl From<char> for Name {
    fn from(name: char) -> Self {
        Self::Short(name)
    }
}

impl From<&str> for Name {
    fn from(name: &str) -> Self {
        Self::Long(name.to_owned())
    }
}

impl From<String> for Name {
    fn from(name: String) -> Self {
        Self::Long(name)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Short(val) => write!(f, "-{}", val),
            Self::Long(val) => write!(f, "--{}", val),
        }
    }
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Word {
    Name(Name),
    Value(String),
}

impl Word {
    pub fn unwrap_name(self) -> Name {
        match self {
            Self::Name(name) => name,
            Self::Value(value) => panic!(
                "called `Word::unwrap_name()` on an `Value` value: {:?}",
                value
            ),
        }
    }
    pub fn unwrap_value(self) -> String {
        match self {
            Self::Name(name) => panic!(
                "called `Word::unwrap_value()` on an `Name` value: {:?}",
                name
            ),
            Self::Value(value) => value,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum WordAssume {
    Argument,
    Long,
    Short,
}

impl WordAssume {
    pub fn guess(string: &str) -> Self {
        let chars: Vec<_> = string.chars().take(2).collect();
        if chars.get(0).map_or(false, |c| c == &'-') {
            chars.get(1).map(|c| c != &'-').and_then(|b| {
                if b {
                    Some(WordAssume::Short)
                } else if chars.get(2).is_some() {
                    Some(WordAssume::Long)
                } else {
                    None
                }
            })
        } else {
            None
        }
        .unwrap_or(WordAssume::Argument)
    }
}

type WordAgentItem = (String, Option<WordAssume>);

#[derive(Debug)]
struct WordAgent {
    end_of_option: bool,
    inner: VecDeque<WordAgentItem>,
}

impl WordAgent {
    pub fn end_of_option(&self) -> bool {
        self.end_of_option
    }
    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = String>,
    {
        self.inner.extend(iter.into_iter().map(|s| (s, None)))
    }
    pub fn get_mut(&mut self) -> &mut VecDeque<WordAgentItem> {
        &mut self.inner
    }
    pub fn get_ref(&self) -> &VecDeque<WordAgentItem> {
        &self.inner
    }
    pub fn new() -> Self {
        WordAgent {
            inner: VecDeque::new(),
            end_of_option: false,
        }
    }
    pub fn shift_special(&mut self) -> bool {
        if let Some((nextstring, None)) = self.inner.front() {
            if !self.end_of_option() && nextstring == "--" {
                self.end_of_option = true;
                self.inner.pop_front().expect("why it's empty");
                true
            } else {
                false
            }
        } else {
            false
        }
    }
}

impl FromIterator<String> for WordAgent {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        let mut s = Self::new();
        s.extend(iter);
        s
    }
}

#[derive(Debug)]
struct WordSession {
    agent: Rc<RefCell<Option<WordAgent>>>,
    stack: VecDeque<WordAgentItem>,
}

impl WordSession {
    pub fn new(agent: Rc<RefCell<Option<WordAgent>>>) -> Self {
        WordSession {
            agent,
            stack: VecDeque::new(),
        }
    }
    pub fn peek_argument(&self) -> Option<String> {
        let mut agent: Option<RefMut<Option<WordAgent>>> = None;
        let stack = if self.stack.is_empty() {
            let agent = agent.insert(self.agent.borrow_mut()).as_mut()?;
            agent.get_mut()
        } else {
            &self.stack
        };
        let (nextstring, _) = stack.front()?;
        Some(nextstring.to_owned())
    }
    fn pop_element_internal(&mut self, force_assume: Option<WordAssume>) -> Option<Word> {
        let mut agent: Option<RefMut<Option<WordAgent>>> = None;
        let stack = if self.stack.is_empty() {
            let agent = agent.insert(self.agent.borrow_mut()).as_mut()?;
            agent.shift_special();
            agent.get_mut()
        } else {
            &mut self.stack
        };
        let (mut nextstring, assume) = stack.pop_front()?;
        let word = match force_assume
            .map(Cow::Owned)
            .unwrap_or_else(|| {
                assume
                    .as_ref()
                    .map(Cow::Borrowed)
                    .unwrap_or_else(|| Cow::Owned(WordAssume::guess(&nextstring)))
            })
            .as_ref()
        {
            WordAssume::Argument => Word::Value(nextstring),
            WordAssume::Long => {
                if assume.is_none() {
                    nextstring.drain(..2);
                }
                let name = match nextstring.char_indices().position(|(_, c)| c == '=') {
                    Some(i) => {
                        let name = nextstring.drain(..i).collect();
                        nextstring.drain(..1);
                        self.stack
                            .push_front((nextstring, Some(WordAssume::Argument)));
                        name
                    }
                    None => nextstring,
                };
                Word::Name(Name::Long(name))
            }
            WordAssume::Short => {
                if assume.is_none() {
                    nextstring.drain(..1);
                }
                let nextchar = nextstring.chars().nth(0)?;
                nextstring.drain(..nextchar.len_utf8());
                if !nextstring.is_empty() {
                    stack.push_front((nextstring, Some(WordAssume::Short)));
                }
                Word::Name(Name::Short(nextchar))
            }
        };
        Some(word)
    }
    fn pop_element(&mut self) -> Option<Word> {
        self.pop_element_internal(None)
    }
    fn pop_argument(&mut self) -> Option<String> {
        self.pop_element_internal(Some(WordAssume::Argument))
            .map(|w| w.unwrap_value())
    }
}

#[derive(Debug)]
struct GetOptionWord {
    agent: Rc<RefCell<Option<WordAgent>>>,
}

impl GetOptionWord {
    pub fn new<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = String>,
    {
        let agent = Rc::new(RefCell::new(Some(WordAgent::from_iter(iter))));
        Self { agent }
    }
}

impl Iterator for GetOptionWord {
    type Item = WordSession;

    fn next(&mut self) -> Option<Self::Item> {
        let agent = self.agent.borrow_mut().take()?;
        if agent.get_ref().is_empty() {
            None
        } else {
            let agent = Rc::new(RefCell::new(Some(agent)));
            self.agent = Rc::clone(&agent);
            Some(WordSession::new(agent))
        }
    }
}

trait Opt {
    fn is_required_argument(&self) -> bool;
    fn output(&self, name: Name) -> &Self::Output;
    type Output: ?Sized;
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct OptInst<T> {
    required_argument: bool,
    output: T,
}

impl<T> OptInst<T> {
    pub fn new(output: T, required_argument: bool) -> Self {
        OptInst {
            required_argument,
            output,
        }
    }
}

impl<T> Opt for OptInst<T> {
    fn is_required_argument(&self) -> bool {
        self.required_argument
    }
    fn output<'a>(&'a self, _name: Name) -> &'a Self::Output {
        &self.output
    }
    type Output = T;
}

#[derive(Debug, Eq, Hash, PartialEq)]
enum ArgumentError {
    ConflictingOptionString(Name),
}

impl fmt::Display for ArgumentError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ConflictingOptionString(name) => write!(f, "conflicting option string: {}", name),
        }
    }
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct OptMass<T>
where
    T: Opt,
{
    inner: BTreeMap<Rc<Name>, Rc<T>>,
}

impl<T> OptMass<T>
where
    T: Opt,
{
    pub fn new() -> Self {
        Self {
            inner: BTreeMap::new(),
        }
    }
    pub fn push<I>(&mut self, opt: T, names: I) -> Result<(), ArgumentError>
    where
        I: IntoIterator<Item = Name>,
    {
        let mut map = BTreeMap::<Rc<Name>, Rc<T>>::new();
        let opt = Rc::new(opt);

        for name in names {
            if self.inner.contains_key(&name) {
                return Err(ArgumentError::ConflictingOptionString(name));
            }
            map.insert(Rc::new(name), opt.clone());
        }
        self.inner.extend(map);
        Ok(())
    }
    pub fn get(&self, name: &Name) -> Option<&Rc<T>> {
        self.inner.get(name)
    }
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Error<'a, T>
where
    T: ?Sized,
{
    InvalidOption(Name),
    RequiresAnArgument(&'a T),
    UnrecognizedArgument(String),
}

struct GetOption<'a, T>
where
    T: Opt,
{
    inner: &'a mut GetOptionWord,
    mass: &'a OptMass<T>,
}

impl<'a, T> GetOption<'a, T>
where
    T: Opt,
{
    pub fn new(getoptword: &'a mut GetOptionWord, optmass: &'a OptMass<T>) -> Self {
        Self {
            inner: getoptword,
            mass: optmass,
        }
    }
}

impl<'a, T> Iterator for GetOption<'a, T>
where
    T: Opt,
{
    type Item = Result<(&'a T::Output, Option<String>), Error<'a, T::Output>>;

    fn next(&mut self) -> Option<Self::Item> {
        for mut session in &mut self.inner {
            match session.pop_element()? {
                Word::Name(name) => {
                    let opt = match self.mass.get(&name) {
                        Some(opt) => opt,
                        None => return Some(Result::Err(Error::InvalidOption(name))),
                    };
                    let argument = if opt.is_required_argument() {
                        Some(match session.pop_argument() {
                            Some(s) => s,
                            None => return Some(Err(Error::RequiresAnArgument(opt.output(name)))),
                        })
                    } else {
                        None
                    };
                    return Some(Ok((opt.output(name), argument)));
                }
                Word::Value(val) => return Some(Err(Error::UnrecognizedArgument(val))),
            }
        }
        None
    }
}

fn print_usage(program: &str) {
    eprintln!("Usage: {} [-v] x y", program)
}

fn main() {
    let mut positional: Vec<String> = Vec::new();
    let mut verbose = false;
    let args: Vec<_> = std::env::args().collect();
    let program = args[0].clone();
    let mut gow = GetOptionWord::new(args.into_iter().skip(1));
    let mut mass = OptMass::new();
    mass.push(
        OptInst::new('v', false),
        [Name::from('v'), Name::from("verbose")],
    )
    .unwrap();
    for opt in GetOption::new(&mut gow, &mass) {
        match opt {
            Ok(opt) => match opt {
                ('v', None) => verbose = true,
                _ => panic!(""),
            },
            Err(e) => match e {
                Error::UnrecognizedArgument(s) => positional.push(s),
                _ => {
                    print_usage(&program);
                    std::process::exit(1);
                }
            },
        }
    }
    let a: [String; 2] = std::convert::TryInto::try_into(positional).unwrap_or_else(|_| {
        print_usage(&program);
        std::process::exit(1);
    });
    let x: u32 = a[0].parse().unwrap_or_else(|_| {
        print_usage(&program);
        std::process::exit(1);
    });
    let y: u32 = a[1].parse().unwrap_or_else(|_| {
        print_usage(&program);
        std::process::exit(1);
    });
    let answer = x.pow(y);
    if verbose {
        println!("{} to the power {} equals {}", x, y, answer);
    } else {
        println!("{}^{} == {}", x, y, answer);
    }
}
