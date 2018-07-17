module Refract.DOM where

import Refract

import Unsafe.Coerce (unsafeCoerce)
  
text :: ∀ s t. String -> FocusedComponent s t
text str = FocusedComponent \_ _ _ -> unsafeCoerce str

a :: ∀ s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
a = mkComponent "a"

a' :: ∀ s t. Array (FocusedComponent s t) -> FocusedComponent s t
a' = a []

abbr :: ∀ s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
abbr = mkComponent "abbr"

abbr' :: ∀ s t. Array (FocusedComponent s t) -> FocusedComponent s t
abbr' = abbr []

address :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
address = mkComponent "address"

address' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
address' = address []

area :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
area = mkComponent "area"

area' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
area' = area []

article :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
article = mkComponent "article"

article' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
article' = article []

aside :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
aside = mkComponent "aside"

aside' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
aside' = aside []

audio :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
audio = mkComponent "audio"

audio' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
audio' = audio []

b :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
b = mkComponent "b"

b' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
b' = b []

base :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
base = mkComponent "base"

base' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
base' = base []

bdi :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
bdi = mkComponent "bdi"

bdi' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
bdi' = bdi []

bdo :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
bdo = mkComponent "bdo"

bdo' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
bdo' = bdo []

big :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
big = mkComponent "big"

big' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
big' = big []

blockquote :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
blockquote = mkComponent "blockquote"

blockquote' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
blockquote' = blockquote []

body :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
body = mkComponent "body"

body' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
body' = body []

br :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
br = mkComponent "br"

br' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
br' = br []

button :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
button = mkComponent "button"

button' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
button' = button []

canvas :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
canvas = mkComponent "canvas"

canvas' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
canvas' = canvas []

caption :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
caption = mkComponent "caption"

caption' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
caption' = caption []

cite :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
cite = mkComponent "cite"

cite' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
cite' = cite []

code :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
code = mkComponent "code"

code' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
code' = code []

col :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
col = mkComponent "col"

col' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
col' = col []

colgroup :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
colgroup = mkComponent "colgroup"

colgroup' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
colgroup' = colgroup []

_data :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
_data = mkComponent "data"

_data' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
_data' = _data []

datalist :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
datalist = mkComponent "datalist"

datalist' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
datalist' = datalist []

dd :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
dd = mkComponent "dd"

dd' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
dd' = dd []

del :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
del = mkComponent "del"

del' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
del' = del []

details :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
details = mkComponent "details"

details' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
details' = details []

dfn :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
dfn = mkComponent "dfn"

dfn' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
dfn' = dfn []

dialog :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
dialog = mkComponent "dialog"

dialog' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
dialog' = dialog []

div :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
div = mkComponent "div"

div' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
div' = div []

dl :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
dl = mkComponent "dl"

dl' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
dl' = dl []

dt :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
dt = mkComponent "dt"

dt' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
dt' = dt []

em :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
em = mkComponent "em"

em' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
em' = em []

embed :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
embed = mkComponent "embed"

embed' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
embed' = embed []

fieldset :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
fieldset = mkComponent "fieldset"

fieldset' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
fieldset' = fieldset []

figcaption :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
figcaption = mkComponent "figcaption"

figcaption' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
figcaption' = figcaption []

figure :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
figure = mkComponent "figure"

figure' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
figure' = figure []

footer :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
footer = mkComponent "footer"

footer' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
footer' = footer []

form :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
form = mkComponent "form"

form' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
form' = form []

h1 :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
h1 = mkComponent "h1"

h1' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
h1' = h1 []

h2 :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
h2 = mkComponent "h2"

h2' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
h2' = h2 []

h3 :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
h3 = mkComponent "h3"

h3' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
h3' = h3 []

h4 :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
h4 = mkComponent "h4"

h4' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
h4' = h4 []

h5 :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
h5 = mkComponent "h5"

h5' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
h5' = h5 []

h6 :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
h6 = mkComponent "h6"

h6' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
h6' = h6 []

head :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
head = mkComponent "head"

head' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
head' = head []

header :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
header = mkComponent "header"

header' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
header' = header []

hr :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
hr = mkComponent "hr"

hr' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
hr' = hr []

html :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
html = mkComponent "html"

html' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
html' = html []

i :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
i = mkComponent "i"

i' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
i' = i []

iframe :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
iframe = mkComponent "iframe"

iframe' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
iframe' = iframe []

img :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
img = mkComponent "img"

img' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
img' = img []

input :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
input = mkComponent "input"

input' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
input' = input []

ins :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
ins = mkComponent "ins"

ins' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
ins' = ins []

kbd :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
kbd = mkComponent "kbd"

kbd' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
kbd' = kbd []

keygen :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
keygen = mkComponent "keygen"

keygen' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
keygen' = keygen []

label :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
label = mkComponent "label"

label' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
label' = label []

legend :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
legend = mkComponent "legend"

legend' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
legend' = legend []

li :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
li = mkComponent "li"

li' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
li' = li []

link :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
link = mkComponent "link"

link' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
link' = body []

main :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
main = mkComponent "main"

main' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
main' = main []

map :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
map = mkComponent "map"

map' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
map' = map []

mark :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
mark = mkComponent "mark"

mark' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
mark' = mark []

menu :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
menu = mkComponent "menu"

menu' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
menu' = menu []

menuitem :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
menuitem = mkComponent "menuitem"

menuitem' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
menuitem' = menuitem []

meta :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
meta = mkComponent "meta"

meta' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
meta' = meta []

meter :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
meter = mkComponent "meter"

meter' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
meter' = meter []

nav :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
nav = mkComponent "nav"

nav' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
nav' = nav []

noscript :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
noscript = mkComponent "noscript"

noscript' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
noscript' = noscript []

object :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
object = mkComponent "object"

object' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
object' = object []

ol :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
ol = mkComponent "ol"

ol' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
ol' = ol []

optgroup :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
optgroup = mkComponent "optgroup"

optgroup' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
optgroup' = optgroup []

option :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
option = mkComponent "option"

option' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
option' = option []

output :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
output = mkComponent "output"

output' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
output' = output []

p :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
p = mkComponent "p"

p' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
p' = p []

param :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
param = mkComponent "param"

param' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
param' = param []

picture :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
picture = mkComponent "picture"

picture' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
picture' = picture []

pre :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
pre = mkComponent "pre"

pre' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
pre' = pre []

progress :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
progress = mkComponent "progress"

progress' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
progress' = progress []

q :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
q = mkComponent "q"

q' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
q' = q []

rp :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
rp = mkComponent "rp"

rp' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
rp' = rp []

rt :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
rt = mkComponent "rt"

rt' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
rt' = rt []

ruby :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
ruby = mkComponent "ruby"

ruby' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
ruby' = ruby []

s :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
s = mkComponent "s"

s' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
s' = s []

samp :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
samp = mkComponent "samp"

samp' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
samp' = samp []

script :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
script = mkComponent "script"

script' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
script' = script []

section :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
section = mkComponent "section"

section' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
section' = section []

select :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
select = mkComponent "select"

select' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
select' = select []

small :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
small = mkComponent "small"

small' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
small' = small []

source :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
source = mkComponent "source"

source' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
source' = source []

span :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
span = mkComponent "span"

span' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
span' = span []

strong :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
strong = mkComponent "strong"

strong' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
strong' = strong []

style :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
style = mkComponent "style"

style' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
style' = style []

sub :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
sub = mkComponent "sub"

sub' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
sub' = sub []

summary :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
summary = mkComponent "summary"

summary' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
summary' = summary []

sup :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
sup = mkComponent "sup"

sup' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
sup' = sup []

table :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
table = mkComponent "table"

table' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
table' = table []

tbody :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
tbody = mkComponent "tbody"

tbody' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
tbody' = tbody []

td :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
td = mkComponent "td"

td' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
td' = td []

textarea :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
textarea = mkComponent "textarea"

textarea' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
textarea' = textarea []

tfoot :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
tfoot = mkComponent "tfoot"

tfoot' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
tfoot' = tfoot []

th :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
th = mkComponent "th"

th' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
th' = th []

thead :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
thead = mkComponent "thead"

thead' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
thead' = thead []

time :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
time = mkComponent "time"

time' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
time' = time []

title :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
title = mkComponent "title"

title' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
title' = title []

tr :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
tr = mkComponent "tr"

tr' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
tr' = tr []

track :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
track = mkComponent "track"

track' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
track' = track []

u :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
u = mkComponent "u"

u' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
u' = u []

ul :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
ul = mkComponent "ul"

ul' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
ul' = ul []

var :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
var = mkComponent "var"

var' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
var' = var []

video :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
video = mkComponent "video"

video' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
video' = video []

wbr :: forall s t. Array (Props s) -> Array (FocusedComponent s t) -> FocusedComponent s t
wbr = mkComponent "body"

wbr' :: forall s t. Array (FocusedComponent s t) -> FocusedComponent s t
wbr' = wbr []
