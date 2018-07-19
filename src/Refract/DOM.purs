module Refract.DOM where

import Refract

import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
  
text :: ∀ s t r. String -> FocusedComponent s r
text str = FocusedComponent \_ _ -> unsafeCoerce str

a :: ∀ s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
a = mkComponent "a"

a' :: ∀ s t r. Array (FocusedComponent s r) -> FocusedComponent s r
a' = a []

abbr :: ∀ s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
abbr = mkComponent "abbr"

abbr' :: ∀ s t r. Array (FocusedComponent s r) -> FocusedComponent s r
abbr' = abbr []

address :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
address = mkComponent "address"

address' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
address' = address []

area :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
area = mkComponent "area"

area' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
area' = area []

article :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
article = mkComponent "article"

article' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
article' = article []

aside :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
aside = mkComponent "aside"

aside' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
aside' = aside []

audio :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
audio = mkComponent "audio"

audio' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
audio' = audio []

b :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
b = mkComponent "b"

b' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
b' = b []

base :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
base = mkComponent "base"

base' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
base' = base []

bdi :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
bdi = mkComponent "bdi"

bdi' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
bdi' = bdi []

bdo :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
bdo = mkComponent "bdo"

bdo' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
bdo' = bdo []

big :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
big = mkComponent "big"

big' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
big' = big []

blockquote :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
blockquote = mkComponent "blockquote"

blockquote' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
blockquote' = blockquote []

body :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
body = mkComponent "body"

body' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
body' = body []

br :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
br = mkComponent "br"

br' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
br' = br []

button :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
button = mkComponent "button"

button' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
button' = button []

canvas :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
canvas = mkComponent "canvas"

canvas' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
canvas' = canvas []

caption :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
caption = mkComponent "caption"

caption' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
caption' = caption []

cite :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
cite = mkComponent "cite"

cite' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
cite' = cite []

code :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
code = mkComponent "code"

code' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
code' = code []

col :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
col = mkComponent "col"

col' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
col' = col []

colgroup :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
colgroup = mkComponent "colgroup"

colgroup' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
colgroup' = colgroup []

_data :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
_data = mkComponent "data"

_data' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
_data' = _data []

datalist :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
datalist = mkComponent "datalist"

datalist' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
datalist' = datalist []

dd :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
dd = mkComponent "dd"

dd' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
dd' = dd []

del :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
del = mkComponent "del"

del' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
del' = del []

details :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
details = mkComponent "details"

details' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
details' = details []

dfn :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
dfn = mkComponent "dfn"

dfn' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
dfn' = dfn []

dialog :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
dialog = mkComponent "dialog"

dialog' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
dialog' = dialog []

div :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
div = mkComponent "div"

div' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
div' = div []

dl :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
dl = mkComponent "dl"

dl' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
dl' = dl []

dt :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
dt = mkComponent "dt"

dt' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
dt' = dt []

em :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
em = mkComponent "em"

em' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
em' = em []

embed :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
embed = mkComponent "embed"

embed' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
embed' = embed []

fieldset :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
fieldset = mkComponent "fieldset"

fieldset' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
fieldset' = fieldset []

figcaption :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
figcaption = mkComponent "figcaption"

figcaption' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
figcaption' = figcaption []

figure :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
figure = mkComponent "figure"

figure' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
figure' = figure []

footer :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
footer = mkComponent "footer"

footer' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
footer' = footer []

form :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
form = mkComponent "form"

form' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
form' = form []

h1 :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
h1 = mkComponent "h1"

h1' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
h1' = h1 []

h2 :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
h2 = mkComponent "h2"

h2' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
h2' = h2 []

h3 :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
h3 = mkComponent "h3"

h3' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
h3' = h3 []

h4 :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
h4 = mkComponent "h4"

h4' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
h4' = h4 []

h5 :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
h5 = mkComponent "h5"

h5' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
h5' = h5 []

h6 :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
h6 = mkComponent "h6"

h6' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
h6' = h6 []

head :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
head = mkComponent "head"

head' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
head' = head []

header :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
header = mkComponent "header"

header' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
header' = header []

hr :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
hr = mkComponent "hr"

hr' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
hr' = hr []

html :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
html = mkComponent "html"

html' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
html' = html []

i :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
i = mkComponent "i"

i' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
i' = i []

iframe :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
iframe = mkComponent "iframe"

iframe' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
iframe' = iframe []

img :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
img = mkComponent "img"

img' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
img' = img []

input :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
input = mkComponent "input"

input' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
input' = input []

ins :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
ins = mkComponent "ins"

ins' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
ins' = ins []

kbd :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
kbd = mkComponent "kbd"

kbd' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
kbd' = kbd []

keygen :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
keygen = mkComponent "keygen"

keygen' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
keygen' = keygen []

label :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
label = mkComponent "label"

label' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
label' = label []

legend :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
legend = mkComponent "legend"

legend' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
legend' = legend []

li :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
li = mkComponent "li"

li' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
li' = li []

link :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
link = mkComponent "link"

link' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
link' = body []

main :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
main = mkComponent "main"

main' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
main' = main []

map :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
map = mkComponent "map"

map' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
map' = map []

mark :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
mark = mkComponent "mark"

mark' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
mark' = mark []

menu :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
menu = mkComponent "menu"

menu' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
menu' = menu []

menuitem :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
menuitem = mkComponent "menuitem"

menuitem' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
menuitem' = menuitem []

meta :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
meta = mkComponent "meta"

meta' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
meta' = meta []

meter :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
meter = mkComponent "meter"

meter' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
meter' = meter []

nav :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
nav = mkComponent "nav"

nav' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
nav' = nav []

noscript :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
noscript = mkComponent "noscript"

noscript' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
noscript' = noscript []

object :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
object = mkComponent "object"

object' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
object' = object []

ol :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
ol = mkComponent "ol"

ol' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
ol' = ol []

optgroup :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
optgroup = mkComponent "optgroup"

optgroup' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
optgroup' = optgroup []

option :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
option = mkComponent "option"

option' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
option' = option []

output :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
output = mkComponent "output"

output' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
output' = output []

p :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
p = mkComponent "p"

p' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
p' = p []

param :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
param = mkComponent "param"

param' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
param' = param []

picture :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
picture = mkComponent "picture"

picture' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
picture' = picture []

pre :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
pre = mkComponent "pre"

pre' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
pre' = pre []

progress :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
progress = mkComponent "progress"

progress' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
progress' = progress []

q :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
q = mkComponent "q"

q' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
q' = q []

rp :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
rp = mkComponent "rp"

rp' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
rp' = rp []

rt :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
rt = mkComponent "rt"

rt' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
rt' = rt []

ruby :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
ruby = mkComponent "ruby"

ruby' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
ruby' = ruby []

s :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
s = mkComponent "s"

s' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
s' = s []

samp :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
samp = mkComponent "samp"

samp' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
samp' = samp []

script :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
script = mkComponent "script"

script' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
script' = script []

section :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
section = mkComponent "section"

section' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
section' = section []

select :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
select = mkComponent "select"

select' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
select' = select []

small :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
small = mkComponent "small"

small' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
small' = small []

source :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
source = mkComponent "source"

source' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
source' = source []

span :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
span = mkComponent "span"

span' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
span' = span []

strong :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
strong = mkComponent "strong"

strong' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
strong' = strong []

style :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
style = mkComponent "style"

style' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
style' = style []

sub :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
sub = mkComponent "sub"

sub' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
sub' = sub []

summary :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
summary = mkComponent "summary"

summary' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
summary' = summary []

sup :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
sup = mkComponent "sup"

sup' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
sup' = sup []

table :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
table = mkComponent "table"

table' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
table' = table []

tbody :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
tbody = mkComponent "tbody"

tbody' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
tbody' = tbody []

td :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
td = mkComponent "td"

td' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
td' = td []

textarea :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
textarea = mkComponent "textarea"

textarea' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
textarea' = textarea []

tfoot :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
tfoot = mkComponent "tfoot"

tfoot' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
tfoot' = tfoot []

th :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
th = mkComponent "th"

th' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
th' = th []

thead :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
thead = mkComponent "thead"

thead' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
thead' = thead []

time :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
time = mkComponent "time"

time' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
time' = time []

title :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
title = mkComponent "title"

title' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
title' = title []

tr :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
tr = mkComponent "tr"

tr' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
tr' = tr []

track :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
track = mkComponent "track"

track' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
track' = track []

u :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
u = mkComponent "u"

u' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
u' = u []

ul :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
ul = mkComponent "ul"

ul' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
ul' = ul []

var :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
var = mkComponent "var"

var' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
var' = var []

video :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
video = mkComponent "video"

video' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
video' = video []

wbr :: forall s t r. Array (Props s r) -> Array (FocusedComponent s r) -> FocusedComponent s r
wbr = mkComponent "body"

wbr' :: forall s t r. Array (FocusedComponent s r) -> FocusedComponent s r
wbr' = wbr []
