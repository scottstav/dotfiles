"""Expand names into spelling variants for STT-tolerant searching.

Speech-to-text picks one spelling of a name, but contacts/emails may use
another. This module maps common Western first-name variant groups so tools
can search for all plausible spellings at once.
"""

# Each tuple is a group of interchangeable spellings (lowercased).
# Order doesn't matter — every entry maps to every other entry in its group.
_VARIANT_GROUPS = [
    # A
    ("adrian", "adrien"),
    ("alison", "allison", "alyson", "allyson"),
    ("amy", "aimee", "aimée"),
    ("andrea", "andria"),
    ("ann", "anne", "anna"),
    ("ashley", "ashlee", "ashleigh", "ashlie"),
    # B
    ("barry", "barrie"),
    ("bret", "brett"),
    ("brian", "bryan", "brion"),
    ("britney", "brittany", "brittney", "brittni"),
    # C
    ("carl", "karl"),
    ("carolyn", "caroline", "karolyn", "karoline"),
    ("catherine", "katherine", "kathryn", "catharine", "katharine", "katarina"),
    ("cathy", "kathy", "kathie", "cathie"),
    ("chris", "kris"),
    ("christina", "kristina", "christeena"),
    ("christine", "kristine", "cristine"),
    ("christy", "kristy", "kristi", "christi"),
    ("clair", "claire", "clare"),
    ("curt", "kurt"),
    ("connor", "conner", "conor"),
    # D
    ("daniel", "danial"),
    ("derick", "derek", "derrick", "derrik"),
    ("dylan", "dillon", "dilan"),
    # E
    ("elena", "elaina", "alaina"),
    ("erica", "erika", "ericka"),
    ("eric", "erik", "erick"),
    ("erin", "eryn", "aryn"),
    # F
    ("felicia", "felisha"),
    ("frances", "francis"),
    # G
    ("gary", "garry"),
    ("geoff", "jeff"),
    ("geoffrey", "jeffrey", "jeffery", "jeffry"),
    ("gerald", "jerald"),
    ("gerry", "jerry"),
    ("gillian", "jillian", "jill", "gill"),
    ("greg", "gregg"),
    ("gregory", "gregorie"),
    # H
    ("hailey", "haley", "hayley", "haylee", "hailee"),
    ("harold", "herald"),
    # I–J
    ("ian", "iain"),
    ("jaclyn", "jaqueline", "jacqueline", "jacquelyn"),
    ("jaime", "jamie"),
    ("jason", "jayson"),
    ("janet", "janette", "jannette"),
    ("janice", "janyce", "janis"),
    ("jean", "jeanne", "jeane"),
    ("jenifer", "jennifer", "jeniffer"),
    ("jenny", "jennie", "jeni"),
    ("jessie", "jesse", "jessi"),
    ("joan", "joanne", "joann", "jo-ann"),
    ("john", "jon"),
    ("jonathan", "johnathan", "jonathon", "johnathon"),
    ("joseph", "josef"),
    ("joshua", "joshuah"),
    ("judith", "judyth"),
    ("judy", "judi", "judie"),
    # K
    ("kayla", "kaila", "cayla"),
    ("kaitlin", "caitlin", "kaitlyn", "caitlyn", "katelin", "catelin"),
    ("kara", "cara"),
    ("karen", "karyn", "caren"),
    ("kelly", "kellie", "kelli"),
    ("kelsey", "kelsie", "kelsi"),
    ("kenneth", "keneth"),
    ("kristin", "kristen", "kristine", "kirsten", "kirstin"),
    ("kylie", "kiley", "kylee"),
    # L
    ("laurie", "lori", "lauri"),
    ("laura", "lora", "lara"),
    ("lauren", "loren", "lauryn"),
    ("lea", "leah", "lia"),
    ("leigh", "lee"),
    ("leslie", "lesley"),
    ("lindsey", "lindsay", "lyndsey", "lyndsay"),
    ("lisa", "leesa"),
    # M
    ("madeline", "madelyn", "madeleine", "madilyn"),
    ("marc", "mark"),
    ("margaret", "margret", "margarite"),
    ("maria", "mariah"),
    ("mary", "marie", "mari"),
    ("mathew", "matthew"),
    ("maureen", "moreen"),
    ("meagan", "megan", "meghan", "meaghan"),
    ("michael", "micheal", "mikael"),
    ("michele", "michelle", "mishelle"),
    ("mike", "mick"),
    # N
    ("nancy", "nancie", "nanci"),
    ("natalie", "nathalie", "nataly"),
    ("neal", "neil", "niel", "neel"),
    ("nicholas", "nicolas", "nikolas"),
    ("nick", "nic"),
    ("nicole", "nichole", "nikole"),
    # O–P
    ("patty", "patti", "pattie"),
    ("patricia", "patrishia"),
    ("peter", "petre"),
    ("philip", "phillip", "filip", "phillipp"),
    # R
    ("rachael", "rachel", "racheal"),
    ("rebecca", "rebekah", "rebeca"),
    ("renee", "renée", "rene"),
    ("ricky", "ricki", "rickie"),
    ("robert", "robbert"),
    ("robin", "robyn"),
    ("roger", "rodger"),
    # S
    ("sara", "sarah"),
    ("sean", "shawn", "shaun"),
    ("sharon", "sharron", "charon"),
    ("shelley", "shelly", "shelli"),
    ("sheryl", "cheryl", "sherrill"),
    ("sophia", "sofia"),
    ("stacey", "stacy", "staci"),
    ("stefan", "stephan"),
    ("stephanie", "stefanie", "stephany", "stefany", "stepheny"),
    ("steven", "stephen", "stevan"),
    ("stuart", "stewart"),
    ("susan", "suzan", "suzanne", "susanne"),
    # T
    ("tara", "terra", "tera"),
    ("teresa", "theresa", "teressa"),
    ("teri", "terri", "terry"),
    ("tiffany", "tiffani", "tiphany"),
    ("tony", "toni"),
    ("tracey", "tracy", "traci", "tracie"),
    # V–Z
    ("valerie", "valery", "valarie"),
    ("vivian", "vivien", "vivienne"),
    ("wendy", "wendi"),
    ("zachary", "zachery", "zackary", "zackery"),
]

# Build a lookup: lowered name -> frozenset of all variants in its group.
_LOOKUP: dict[str, frozenset[str]] = {}
for _group in _VARIANT_GROUPS:
    _fset = frozenset(_group)
    for _name in _group:
        # A name may appear in multiple groups (overlap is fine — union them).
        if _name in _LOOKUP:
            _LOOKUP[_name] = _LOOKUP[_name] | _fset
        else:
            _LOOKUP[_name] = _fset


def get_variants(name: str) -> list[str]:
    """Return all known spelling variants for a name, including itself.

    The input name is always included in the output.  If no variants are
    known, returns a single-element list with the original name.
    """
    key = name.lower().strip()
    if key in _LOOKUP:
        return sorted(_LOOKUP[key])
    return [key]
