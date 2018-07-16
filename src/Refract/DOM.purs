module Refract.DOM where

import Refract

import Unsafe.Coerce (unsafeCoerce)
  
text :: ∀ st s. String -> FocusedComponent st s
text str = FocusedComponent \_ _ _ -> unsafeCoerce str

a :: ∀ st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
a = mkComponent "a"

a' :: ∀ st s. Array (FocusedComponent st s) -> FocusedComponent st s
a' = a []

abbr :: ∀ st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
abbr = mkComponent "abbr"

abbr' :: ∀ st s. Array (FocusedComponent st s) -> FocusedComponent st s
abbr' = abbr []

address :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
address = mkComponent "address"

address' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
address' = address []

area :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
area = mkComponent "area"

area' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
area' = area []

article :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
article = mkComponent "article"

article' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
article' = article []

aside :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
aside = mkComponent "aside"

aside' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
aside' = aside []

audio :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
audio = mkComponent "audio"

audio' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
audio' = audio []

b :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
b = mkComponent "b"

b' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
b' = b []

base :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
base = mkComponent "base"

base' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
base' = base []

bdi :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
bdi = mkComponent "bdi"

bdi' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
bdi' = bdi []

bdo :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
bdo = mkComponent "bdo"

bdo' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
bdo' = bdo []

big :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
big = mkComponent "big"

big' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
big' = big []

blockquote :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
blockquote = mkComponent "blockquote"

blockquote' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
blockquote' = blockquote []

body :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
body = mkComponent "body"

body' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
body' = body []

br :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
br = mkComponent "br"

br' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
br' = br []

button :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
button = mkComponent "button"

button' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
button' = button []

canvas :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
canvas = mkComponent "canvas"

canvas' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
canvas' = canvas []

caption :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
caption = mkComponent "caption"

caption' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
caption' = caption []

cite :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
cite = mkComponent "cite"

cite' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
cite' = cite []

code :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
code = mkComponent "code"

code' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
code' = code []

col :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
col = mkComponent "col"

col' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
col' = col []

colgroup :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
colgroup = mkComponent "colgroup"

colgroup' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
colgroup' = colgroup []

_data :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
_data = mkComponent "data"

_data' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
_data' = _data []

datalist :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
datalist = mkComponent "datalist"

datalist' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
datalist' = datalist []

dd :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
dd = mkComponent "dd"

dd' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
dd' = dd []

del :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
del = mkComponent "del"

del' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
del' = del []

details :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
details = mkComponent "details"

details' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
details' = details []

dfn :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
dfn = mkComponent "dfn"

dfn' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
dfn' = dfn []

dialog :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
dialog = mkComponent "dialog"

dialog' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
dialog' = dialog []

div :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
div = mkComponent "div"

div' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
div' = div []

dl :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
dl = mkComponent "dl"

dl' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
dl' = dl []

dt :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
dt = mkComponent "dt"

dt' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
dt' = dt []

em :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
em = mkComponent "em"

em' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
em' = em []

embed :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
embed = mkComponent "embed"

embed' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
embed' = embed []

fieldset :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
fieldset = mkComponent "fieldset"

fieldset' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
fieldset' = fieldset []

figcaption :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
figcaption = mkComponent "figcaption"

figcaption' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
figcaption' = figcaption []

figure :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
figure = mkComponent "figure"

figure' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
figure' = figure []

footer :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
footer = mkComponent "footer"

footer' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
footer' = footer []

form :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
form = mkComponent "form"

form' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
form' = form []

h1 :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
h1 = mkComponent "h1"

h1' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
h1' = h1 []

h2 :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
h2 = mkComponent "h2"

h2' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
h2' = h2 []

h3 :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
h3 = mkComponent "h3"

h3' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
h3' = h3 []

h4 :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
h4 = mkComponent "h4"

h4' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
h4' = h4 []

h5 :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
h5 = mkComponent "h5"

h5' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
h5' = h5 []

h6 :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
h6 = mkComponent "h6"

h6' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
h6' = h6 []

head :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
head = mkComponent "head"

head' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
head' = head []

header :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
header = mkComponent "header"

header' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
header' = header []

hr :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
hr = mkComponent "hr"

hr' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
hr' = hr []

html :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
html = mkComponent "html"

html' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
html' = html []

i :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
i = mkComponent "i"

i' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
i' = i []

iframe :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
iframe = mkComponent "iframe"

iframe' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
iframe' = iframe []

img :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
img = mkComponent "img"

img' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
img' = img []

input :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
input = mkComponent "input"

input' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
input' = input []

ins :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
ins = mkComponent "ins"

ins' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
ins' = ins []

kbd :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
kbd = mkComponent "kbd"

kbd' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
kbd' = kbd []

keygen :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
keygen = mkComponent "keygen"

keygen' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
keygen' = keygen []

label :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
label = mkComponent "label"

label' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
label' = label []

legend :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
legend = mkComponent "legend"

legend' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
legend' = legend []

li :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
li = mkComponent "li"

li' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
li' = li []

link :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
link = mkComponent "link"

link' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
link' = body []

main :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
main = mkComponent "main"

main' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
main' = main []

map :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
map = mkComponent "map"

map' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
map' = map []

mark :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
mark = mkComponent "mark"

mark' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
mark' = mark []

menu :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
menu = mkComponent "menu"

menu' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
menu' = menu []

menuitem :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
menuitem = mkComponent "menuitem"

menuitem' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
menuitem' = menuitem []

meta :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
meta = mkComponent "meta"

meta' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
meta' = meta []

meter :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
meter = mkComponent "meter"

meter' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
meter' = meter []

nav :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
nav = mkComponent "nav"

nav' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
nav' = nav []

noscript :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
noscript = mkComponent "noscript"

noscript' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
noscript' = noscript []

object :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
object = mkComponent "object"

object' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
object' = object []

ol :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
ol = mkComponent "ol"

ol' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
ol' = ol []

optgroup :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
optgroup = mkComponent "optgroup"

optgroup' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
optgroup' = optgroup []

option :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
option = mkComponent "option"

option' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
option' = option []

output :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
output = mkComponent "output"

output' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
output' = output []

p :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
p = mkComponent "p"

p' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
p' = p []

param :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
param = mkComponent "param"

param' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
param' = param []

picture :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
picture = mkComponent "picture"

picture' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
picture' = picture []

pre :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
pre = mkComponent "pre"

pre' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
pre' = pre []

progress :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
progress = mkComponent "progress"

progress' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
progress' = progress []

q :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
q = mkComponent "q"

q' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
q' = q []

rp :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
rp = mkComponent "rp"

rp' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
rp' = rp []

rt :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
rt = mkComponent "rt"

rt' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
rt' = rt []

ruby :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
ruby = mkComponent "ruby"

ruby' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
ruby' = ruby []

s :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
s = mkComponent "s"

s' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
s' = s []

samp :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
samp = mkComponent "samp"

samp' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
samp' = samp []

script :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
script = mkComponent "script"

script' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
script' = script []

section :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
section = mkComponent "section"

section' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
section' = section []

select :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
select = mkComponent "select"

select' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
select' = select []

small :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
small = mkComponent "small"

small' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
small' = small []

source :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
source = mkComponent "source"

source' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
source' = source []

span :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
span = mkComponent "span"

span' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
span' = span []

strong :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
strong = mkComponent "strong"

strong' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
strong' = strong []

style :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
style = mkComponent "style"

style' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
style' = style []

sub :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
sub = mkComponent "sub"

sub' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
sub' = sub []

summary :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
summary = mkComponent "summary"

summary' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
summary' = summary []

sup :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
sup = mkComponent "sup"

sup' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
sup' = sup []

table :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
table = mkComponent "table"

table' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
table' = table []

tbody :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
tbody = mkComponent "tbody"

tbody' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
tbody' = tbody []

td :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
td = mkComponent "td"

td' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
td' = td []

textarea :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
textarea = mkComponent "textarea"

textarea' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
textarea' = textarea []

tfoot :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
tfoot = mkComponent "tfoot"

tfoot' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
tfoot' = tfoot []

th :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
th = mkComponent "th"

th' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
th' = th []

thead :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
thead = mkComponent "thead"

thead' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
thead' = thead []

time :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
time = mkComponent "time"

time' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
time' = time []

title :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
title = mkComponent "title"

title' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
title' = title []

tr :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
tr = mkComponent "tr"

tr' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
tr' = tr []

track :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
track = mkComponent "track"

track' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
track' = track []

u :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
u = mkComponent "u"

u' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
u' = u []

ul :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
ul = mkComponent "ul"

ul' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
ul' = ul []

var :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
var = mkComponent "var"

var' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
var' = var []

video :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
video = mkComponent "video"

video' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
video' = video []

wbr :: forall st s. Array (Props st s) -> Array (FocusedComponent st s) -> FocusedComponent st s
wbr = mkComponent "body"

wbr' :: forall st s. Array (FocusedComponent st s) -> FocusedComponent st s
wbr' = wbr []
