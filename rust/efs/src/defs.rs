pub type Save<'a> = Vec<Nation<'a>>;
pub type PResult<T> = Result<T, PError>;
pub type Tag = [char; 3];

#[derive(Debug)]
pub struct PError
{
    pub desc : &'static str,
    pub bad_token : Option<Token>,
    pub expected : Option<Token>
}

impl PError
{
    pub fn new(desc : &'static str,
           bad_token : Option<Token>,
           expected : Option<Token>) -> PError
    {
        PError
        {
            desc : desc,
            bad_token : bad_token,
            expected : expected
        }
    }
}

#[derive(Debug)]
pub struct Nation<'a>
{
    pub primary  : &'a str,
    pub accepted : Vec<&'a str>,
    pub money    : f64,
    pub states   : Vec<State<'a>>,
    pub armies   : Vec<Army>,
    pub navies   : Vec<Navy>,
    pub tech     : Tech
}

// don't need to actually worry about *which* techs, only record the number
// important techs can be selected for manually
#[derive(Debug)]
pub struct Tech
{
    pub arm : u64,
    pub nav : u64,
    pub com : u64,
    pub cul : u64,
    pub ind : u64
}

#[derive(Debug)]
pub struct Pop<'a>
{
    pub size    : u64,
    pub money   : f64,
    pub poptype : Poptype,
    pub culture : &'a str, // intern strings somewhere?
    pub mil     : f64,
    pub con     : f64,
    // maybe expand with needs as well, at some stage?
}

#[derive(Debug)]
pub enum Poptype
{
    // working class
    Slave,
    Farmer,
    Labourer,
    Soldier,
    Craftsman,

    // middle class
    Clerk,
    Officer,
    Clergy,
    Bureaucrat,
    Artisan,
    
    // upper class
    Aristocrat,
    Capitalist
}

#[derive(Debug)]
pub struct Province<'a>
{
    pub id         : u64,
    pub owner      : Tag,
    pub controller : Tag,
    pub cores      : Vec<Tag>,
    pub pops       : Vec<Pop<'a>>,
}

#[derive(Debug)]
pub struct State<'a>
{
    #[allow(unused)] pub factories : Vec<Factory>,
    pub provinces : Vec<Province<'a>>,
}

#[derive(Debug)]
#[allow(unused)]
pub struct Factory
{
    pub level : u64,
    pub needs : [(Good, u64); 4], // no factory actually needs more than this
    pub makes : Good // amount can be inferred
}

#[derive(Debug)]
pub enum Good
{
    Artillery,
    CannedFood,
    Grain,
    Ammunition,
    // etc
}

// Can probably avoid hardcoding these
// but eh
// basically just [u64] of counts
// include the tag as 3 utf8 codepoints as well
#[derive(Debug)]
pub struct Army
{
    pub tag : [char; 3], // 
    pub irr : u64,
    pub cav : u64,
    pub inf : u64,
    pub hus : u64,
    pub cui : u64,
    pub dra : u64,
    pub art : u64,
    pub eng : u64,
    pub gua : u64,
    pub arm : u64,
    pub pla : u64
}

#[derive(Debug)]
pub struct Navy
{
    pub tag : [char; 3], 
    pub cli : u64,
    pub fri : u64,
    pub man : u64,
    pub com : u64,
    pub tra : u64,
    pub mon : u64,
    pub iro : u64,
    pub cru : u64,
    pub bat : u64,
    pub dre : u64
}

#[derive(Debug)]
pub enum Mil
{
    A(Army),
    N(Navy)
}

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Token
{
    Delim, // for spaces, newlines, tabs etc etc etc
    OpenBrace, // literally '{'
    CloseBrace, // literally '}'
    Eq, // literally '='
    Int(i64), // any integer number
    Float(f64), // any floating point number
    Date(i64,i64,i64), // for a YYYY.MM.DD date
    Str(String), // a string that was enclosed in ""
    Ident(String), // any other token
    Placeholder // for an unknown token (for initialising arrays)
}


// never do this in future
// todo: replace with same thing as navy
#[derive(Debug, PartialEq, Eq)]
pub enum Regtype
{
    Irr,
    Cav,
    Inf,
    Hus,
    Cui,
    Dra,
    Art,
    Eng,
    Gua,
    Arm,
    Pla
}
