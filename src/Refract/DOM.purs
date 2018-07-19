module Refract.DOM where

import Refract

import Unsafe.Coerce (unsafeCoerce)
  
text :: ∀ p s r. String -> p -> s -> Component' s r
text str _ _ = Component' \_ -> unsafeCoerce str

a :: ∀ p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
a = mkComponent "a"

a' :: ∀ p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
a' = a []

abbr :: ∀ p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
abbr = mkComponent "abbr"

abbr' :: ∀ p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
abbr' = abbr []

address :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
address = mkComponent "address"

address' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
address' = address []

area :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
area = mkComponent "area"

area' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
area' = area []

article :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
article = mkComponent "article"

article' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
article' = article []

aside :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
aside = mkComponent "aside"

aside' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
aside' = aside []

audio :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
audio = mkComponent "audio"

audio' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
audio' = audio []

b :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
b = mkComponent "b"

b' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
b' = b []

base :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
base = mkComponent "base"

base' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
base' = base []

bdi :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
bdi = mkComponent "bdi"

bdi' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
bdi' = bdi []

bdo :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
bdo = mkComponent "bdo"

bdo' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
bdo' = bdo []

big :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
big = mkComponent "big"

big' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
big' = big []

blockquote :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
blockquote = mkComponent "blockquote"

blockquote' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
blockquote' = blockquote []

body :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
body = mkComponent "body"

body' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
body' = body []

br :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
br = mkComponent "br"

br' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
br' = br []

button :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
button = mkComponent "button"

button' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
button' = button []

canvas :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
canvas = mkComponent "canvas"

canvas' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
canvas' = canvas []

caption :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
caption = mkComponent "caption"

caption' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
caption' = caption []

cite :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
cite = mkComponent "cite"

cite' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
cite' = cite []

code :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
code = mkComponent "code"

code' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
code' = code []

col :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
col = mkComponent "col"

col' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
col' = col []

colgroup :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
colgroup = mkComponent "colgroup"

colgroup' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
colgroup' = colgroup []

_data :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
_data = mkComponent "data"

_data' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
_data' = _data []

datalist :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
datalist = mkComponent "datalist"

datalist' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
datalist' = datalist []

dd :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
dd = mkComponent "dd"

dd' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
dd' = dd []

del :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
del = mkComponent "del"

del' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
del' = del []

details :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
details = mkComponent "details"

details' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
details' = details []

dfn :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
dfn = mkComponent "dfn"

dfn' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
dfn' = dfn []

dialog :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
dialog = mkComponent "dialog"

dialog' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
dialog' = dialog []

div :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
div = mkComponent "div"

div' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
div' = div []

dl :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
dl = mkComponent "dl"

dl' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
dl' = dl []

dt :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
dt = mkComponent "dt"

dt' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
dt' = dt []

em :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
em = mkComponent "em"

em' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
em' = em []

embed :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
embed = mkComponent "embed"

embed' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
embed' = embed []

fieldset :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
fieldset = mkComponent "fieldset"

fieldset' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
fieldset' = fieldset []

figcaption :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
figcaption = mkComponent "figcaption"

figcaption' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
figcaption' = figcaption []

figure :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
figure = mkComponent "figure"

figure' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
figure' = figure []

footer :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
footer = mkComponent "footer"

footer' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
footer' = footer []

form :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
form = mkComponent "form"

form' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
form' = form []

h1 :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
h1 = mkComponent "h1"

h1' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
h1' = h1 []

h2 :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
h2 = mkComponent "h2"

h2' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
h2' = h2 []

h3 :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
h3 = mkComponent "h3"

h3' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
h3' = h3 []

h4 :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
h4 = mkComponent "h4"

h4' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
h4' = h4 []

h5 :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
h5 = mkComponent "h5"

h5' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
h5' = h5 []

h6 :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
h6 = mkComponent "h6"

h6' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
h6' = h6 []

head :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
head = mkComponent "head"

head' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
head' = head []

header :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
header = mkComponent "header"

header' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
header' = header []

hr :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
hr = mkComponent "hr"

hr' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
hr' = hr []

html :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
html = mkComponent "html"

html' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
html' = html []

i :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
i = mkComponent "i"

i' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
i' = i []

iframe :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
iframe = mkComponent "iframe"

iframe' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
iframe' = iframe []

img :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
img = mkComponent "img"

img' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
img' = img []

input :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
input = mkComponent "input"

input' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
input' = input []

ins :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
ins = mkComponent "ins"

ins' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
ins' = ins []

kbd :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
kbd = mkComponent "kbd"

kbd' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
kbd' = kbd []

keygen :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
keygen = mkComponent "keygen"

keygen' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
keygen' = keygen []

label :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
label = mkComponent "label"

label' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
label' = label []

legend :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
legend = mkComponent "legend"

legend' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
legend' = legend []

li :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
li = mkComponent "li"

li' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
li' = li []

link :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
link = mkComponent "link"

link' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
link' = body []

main :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
main = mkComponent "main"

main' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
main' = main []

map :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
map = mkComponent "map"

map' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
map' = map []

mark :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
mark = mkComponent "mark"

mark' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
mark' = mark []

menu :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
menu = mkComponent "menu"

menu' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
menu' = menu []

menuitem :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
menuitem = mkComponent "menuitem"

menuitem' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
menuitem' = menuitem []

meta :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
meta = mkComponent "meta"

meta' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
meta' = meta []

meter :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
meter = mkComponent "meter"

meter' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
meter' = meter []

nav :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
nav = mkComponent "nav"

nav' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
nav' = nav []

noscript :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
noscript = mkComponent "noscript"

noscript' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
noscript' = noscript []

object :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
object = mkComponent "object"

object' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
object' = object []

ol :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
ol = mkComponent "ol"

ol' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
ol' = ol []

optgroup :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
optgroup = mkComponent "optgroup"

optgroup' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
optgroup' = optgroup []

option :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
option = mkComponent "option"

option' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
option' = option []

output :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
output = mkComponent "output"

output' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
output' = output []

p :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
p = mkComponent "p"

p' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
p' = p []

param :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
param = mkComponent "param"

param' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
param' = param []

picture :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
picture = mkComponent "picture"

picture' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
picture' = picture []

pre :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
pre = mkComponent "pre"

pre' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
pre' = pre []

progress :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
progress = mkComponent "progress"

progress' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
progress' = progress []

q :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
q = mkComponent "q"

q' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
q' = q []

rp :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
rp = mkComponent "rp"

rp' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
rp' = rp []

rt :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
rt = mkComponent "rt"

rt' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
rt' = rt []

ruby :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
ruby = mkComponent "ruby"

ruby' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
ruby' = ruby []

s :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
s = mkComponent "s"

s' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
s' = s []

samp :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
samp = mkComponent "samp"

samp' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
samp' = samp []

script :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
script = mkComponent "script"

script' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
script' = script []

section :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
section = mkComponent "section"

section' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
section' = section []

select :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
select = mkComponent "select"

select' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
select' = select []

small :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
small = mkComponent "small"

small' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
small' = small []

source :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
source = mkComponent "source"

source' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
source' = source []

span :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
span = mkComponent "span"

span' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
span' = span []

strong :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
strong = mkComponent "strong"

strong' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
strong' = strong []

style :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
style = mkComponent "style"

style' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
style' = style []

sub :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
sub = mkComponent "sub"

sub' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
sub' = sub []

summary :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
summary = mkComponent "summary"

summary' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
summary' = summary []

sup :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
sup = mkComponent "sup"

sup' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
sup' = sup []

table :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
table = mkComponent "table"

table' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
table' = table []

tbody :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
tbody = mkComponent "tbody"

tbody' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
tbody' = tbody []

td :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
td = mkComponent "td"

td' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
td' = td []

textarea :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
textarea = mkComponent "textarea"

textarea' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
textarea' = textarea []

tfoot :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
tfoot = mkComponent "tfoot"

tfoot' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
tfoot' = tfoot []

th :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
th = mkComponent "th"

th' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
th' = th []

thead :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
thead = mkComponent "thead"

thead' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
thead' = thead []

time :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
time = mkComponent "time"

time' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
time' = time []

title :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
title = mkComponent "title"

title' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
title' = title []

tr :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
tr = mkComponent "tr"

tr' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
tr' = tr []

track :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
track = mkComponent "track"

track' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
track' = track []

u :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
u = mkComponent "u"

u' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
u' = u []

ul :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
ul = mkComponent "ul"

ul' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
ul' = ul []

var :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
var = mkComponent "var"

var' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
var' = var []

video :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
video = mkComponent "video"

video' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
video' = video []

wbr :: forall p s r. Array (Props s r) -> Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
wbr = mkComponent "body"

wbr' :: forall p s r. Array ({} -> s -> Component' s r) -> p -> s -> Component' s r
wbr' = wbr []
