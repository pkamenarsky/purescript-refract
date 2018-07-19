module Refract.DOM where

import Refract

import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
  
text :: ∀ s t r. String -> FocusedComponent s t r
text str = FocusedComponent \_ _ _ -> unsafeCoerce str

a :: ∀ s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
a = mkComponent "a"

a' :: ∀ s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
a' = a []

abbr :: ∀ s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
abbr = mkComponent "abbr"

abbr' :: ∀ s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
abbr' = abbr []

address :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
address = mkComponent "address"

address' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
address' = address []

area :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
area = mkComponent "area"

area' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
area' = area []

article :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
article = mkComponent "article"

article' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
article' = article []

aside :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
aside = mkComponent "aside"

aside' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
aside' = aside []

audio :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
audio = mkComponent "audio"

audio' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
audio' = audio []

b :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
b = mkComponent "b"

b' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
b' = b []

base :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
base = mkComponent "base"

base' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
base' = base []

bdi :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
bdi = mkComponent "bdi"

bdi' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
bdi' = bdi []

bdo :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
bdo = mkComponent "bdo"

bdo' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
bdo' = bdo []

big :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
big = mkComponent "big"

big' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
big' = big []

blockquote :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
blockquote = mkComponent "blockquote"

blockquote' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
blockquote' = blockquote []

body :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
body = mkComponent "body"

body' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
body' = body []

br :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
br = mkComponent "br"

br' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
br' = br []

button :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
button = mkComponent "button"

button' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
button' = button []

canvas :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
canvas = mkComponent "canvas"

canvas' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
canvas' = canvas []

caption :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
caption = mkComponent "caption"

caption' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
caption' = caption []

cite :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
cite = mkComponent "cite"

cite' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
cite' = cite []

code :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
code = mkComponent "code"

code' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
code' = code []

col :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
col = mkComponent "col"

col' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
col' = col []

colgroup :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
colgroup = mkComponent "colgroup"

colgroup' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
colgroup' = colgroup []

_data :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
_data = mkComponent "data"

_data' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
_data' = _data []

datalist :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
datalist = mkComponent "datalist"

datalist' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
datalist' = datalist []

dd :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
dd = mkComponent "dd"

dd' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
dd' = dd []

del :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
del = mkComponent "del"

del' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
del' = del []

details :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
details = mkComponent "details"

details' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
details' = details []

dfn :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
dfn = mkComponent "dfn"

dfn' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
dfn' = dfn []

dialog :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
dialog = mkComponent "dialog"

dialog' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
dialog' = dialog []

div :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
div = mkComponent "div"

div' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
div' = div []

dl :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
dl = mkComponent "dl"

dl' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
dl' = dl []

dt :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
dt = mkComponent "dt"

dt' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
dt' = dt []

em :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
em = mkComponent "em"

em' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
em' = em []

embed :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
embed = mkComponent "embed"

embed' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
embed' = embed []

fieldset :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
fieldset = mkComponent "fieldset"

fieldset' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
fieldset' = fieldset []

figcaption :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
figcaption = mkComponent "figcaption"

figcaption' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
figcaption' = figcaption []

figure :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
figure = mkComponent "figure"

figure' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
figure' = figure []

footer :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
footer = mkComponent "footer"

footer' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
footer' = footer []

form :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
form = mkComponent "form"

form' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
form' = form []

h1 :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
h1 = mkComponent "h1"

h1' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
h1' = h1 []

h2 :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
h2 = mkComponent "h2"

h2' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
h2' = h2 []

h3 :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
h3 = mkComponent "h3"

h3' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
h3' = h3 []

h4 :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
h4 = mkComponent "h4"

h4' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
h4' = h4 []

h5 :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
h5 = mkComponent "h5"

h5' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
h5' = h5 []

h6 :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
h6 = mkComponent "h6"

h6' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
h6' = h6 []

head :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
head = mkComponent "head"

head' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
head' = head []

header :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
header = mkComponent "header"

header' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
header' = header []

hr :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
hr = mkComponent "hr"

hr' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
hr' = hr []

html :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
html = mkComponent "html"

html' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
html' = html []

i :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
i = mkComponent "i"

i' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
i' = i []

iframe :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
iframe = mkComponent "iframe"

iframe' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
iframe' = iframe []

img :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
img = mkComponent "img"

img' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
img' = img []

input :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
input = mkComponent "input"

input' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
input' = input []

ins :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
ins = mkComponent "ins"

ins' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
ins' = ins []

kbd :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
kbd = mkComponent "kbd"

kbd' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
kbd' = kbd []

keygen :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
keygen = mkComponent "keygen"

keygen' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
keygen' = keygen []

label :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
label = mkComponent "label"

label' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
label' = label []

legend :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
legend = mkComponent "legend"

legend' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
legend' = legend []

li :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
li = mkComponent "li"

li' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
li' = li []

link :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
link = mkComponent "link"

link' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
link' = body []

main :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
main = mkComponent "main"

main' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
main' = main []

map :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
map = mkComponent "map"

map' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
map' = map []

mark :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
mark = mkComponent "mark"

mark' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
mark' = mark []

menu :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
menu = mkComponent "menu"

menu' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
menu' = menu []

menuitem :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
menuitem = mkComponent "menuitem"

menuitem' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
menuitem' = menuitem []

meta :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
meta = mkComponent "meta"

meta' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
meta' = meta []

meter :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
meter = mkComponent "meter"

meter' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
meter' = meter []

nav :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
nav = mkComponent "nav"

nav' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
nav' = nav []

noscript :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
noscript = mkComponent "noscript"

noscript' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
noscript' = noscript []

object :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
object = mkComponent "object"

object' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
object' = object []

ol :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
ol = mkComponent "ol"

ol' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
ol' = ol []

optgroup :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
optgroup = mkComponent "optgroup"

optgroup' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
optgroup' = optgroup []

option :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
option = mkComponent "option"

option' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
option' = option []

output :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
output = mkComponent "output"

output' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
output' = output []

p :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
p = mkComponent "p"

p' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
p' = p []

param :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
param = mkComponent "param"

param' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
param' = param []

picture :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
picture = mkComponent "picture"

picture' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
picture' = picture []

pre :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
pre = mkComponent "pre"

pre' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
pre' = pre []

progress :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
progress = mkComponent "progress"

progress' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
progress' = progress []

q :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
q = mkComponent "q"

q' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
q' = q []

rp :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
rp = mkComponent "rp"

rp' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
rp' = rp []

rt :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
rt = mkComponent "rt"

rt' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
rt' = rt []

ruby :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
ruby = mkComponent "ruby"

ruby' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
ruby' = ruby []

s :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
s = mkComponent "s"

s' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
s' = s []

samp :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
samp = mkComponent "samp"

samp' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
samp' = samp []

script :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
script = mkComponent "script"

script' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
script' = script []

section :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
section = mkComponent "section"

section' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
section' = section []

select :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
select = mkComponent "select"

select' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
select' = select []

small :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
small = mkComponent "small"

small' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
small' = small []

source :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
source = mkComponent "source"

source' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
source' = source []

span :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
span = mkComponent "span"

span' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
span' = span []

strong :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
strong = mkComponent "strong"

strong' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
strong' = strong []

style :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
style = mkComponent "style"

style' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
style' = style []

sub :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
sub = mkComponent "sub"

sub' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
sub' = sub []

summary :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
summary = mkComponent "summary"

summary' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
summary' = summary []

sup :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
sup = mkComponent "sup"

sup' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
sup' = sup []

table :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
table = mkComponent "table"

table' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
table' = table []

tbody :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
tbody = mkComponent "tbody"

tbody' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
tbody' = tbody []

td :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
td = mkComponent "td"

td' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
td' = td []

textarea :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
textarea = mkComponent "textarea"

textarea' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
textarea' = textarea []

tfoot :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
tfoot = mkComponent "tfoot"

tfoot' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
tfoot' = tfoot []

th :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
th = mkComponent "th"

th' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
th' = th []

thead :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
thead = mkComponent "thead"

thead' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
thead' = thead []

time :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
time = mkComponent "time"

time' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
time' = time []

title :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
title = mkComponent "title"

title' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
title' = title []

tr :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
tr = mkComponent "tr"

tr' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
tr' = tr []

track :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
track = mkComponent "track"

track' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
track' = track []

u :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
u = mkComponent "u"

u' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
u' = u []

ul :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
ul = mkComponent "ul"

ul' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
ul' = ul []

var :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
var = mkComponent "var"

var' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
var' = var []

video :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
video = mkComponent "video"

video' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
video' = video []

wbr :: forall s t r. Array (Props s r) -> Array (FocusedComponent s t r) -> FocusedComponent s t r
wbr = mkComponent "body"

wbr' :: forall s t r. Array (FocusedComponent s t r) -> FocusedComponent s t r
wbr' = wbr []
