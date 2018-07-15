module Refract.DOM where

import Refract

import Unsafe.Coerce (unsafeCoerce)
  
text :: ∀ st. String -> Component st
text str _ _ = unsafeCoerce str

a :: ∀ st. Array (Props st) -> Array (Component st) -> Component st
a = mkComponent "a"

a' :: ∀ st. Array (Component st) -> Component st
a' = a []

abbr :: ∀ st. Array (Props st) -> Array (Component st) -> Component st
abbr = mkComponent "abbr"

abbr' :: ∀ st. Array (Component st) -> Component st
abbr' = abbr []

address :: forall st. Array (Props st) -> Array (Component st) -> Component st
address = mkComponent "address"

address' :: forall st. Array (Component st) -> Component st
address' = address []

area :: forall st. Array (Props st) -> Array (Component st) -> Component st
area = mkComponent "area"

area' :: forall st. Array (Component st) -> Component st
area' = area []

article :: forall st. Array (Props st) -> Array (Component st) -> Component st
article = mkComponent "article"

article' :: forall st. Array (Component st) -> Component st
article' = article []

aside :: forall st. Array (Props st) -> Array (Component st) -> Component st
aside = mkComponent "aside"

aside' :: forall st. Array (Component st) -> Component st
aside' = aside []

audio :: forall st. Array (Props st) -> Array (Component st) -> Component st
audio = mkComponent "audio"

audio' :: forall st. Array (Component st) -> Component st
audio' = audio []

b :: forall st. Array (Props st) -> Array (Component st) -> Component st
b = mkComponent "b"

b' :: forall st. Array (Component st) -> Component st
b' = b []

base :: forall st. Array (Props st) -> Array (Component st) -> Component st
base = mkComponent "base"

base' :: forall st. Array (Component st) -> Component st
base' = base []

bdi :: forall st. Array (Props st) -> Array (Component st) -> Component st
bdi = mkComponent "bdi"

bdi' :: forall st. Array (Component st) -> Component st
bdi' = bdi []

bdo :: forall st. Array (Props st) -> Array (Component st) -> Component st
bdo = mkComponent "bdo"

bdo' :: forall st. Array (Component st) -> Component st
bdo' = bdo []

big :: forall st. Array (Props st) -> Array (Component st) -> Component st
big = mkComponent "big"

big' :: forall st. Array (Component st) -> Component st
big' = big []

blockquote :: forall st. Array (Props st) -> Array (Component st) -> Component st
blockquote = mkComponent "blockquote"

blockquote' :: forall st. Array (Component st) -> Component st
blockquote' = blockquote []

body :: forall st. Array (Props st) -> Array (Component st) -> Component st
body = mkComponent "body"

body' :: forall st. Array (Component st) -> Component st
body' = body []

br :: forall st. Array (Props st) -> Array (Component st) -> Component st
br = mkComponent "br"

br' :: forall st. Array (Component st) -> Component st
br' = br []

button :: forall st. Array (Props st) -> Array (Component st) -> Component st
button = mkComponent "button"

button' :: forall st. Array (Component st) -> Component st
button' = button []

canvas :: forall st. Array (Props st) -> Array (Component st) -> Component st
canvas = mkComponent "canvas"

canvas' :: forall st. Array (Component st) -> Component st
canvas' = canvas []

caption :: forall st. Array (Props st) -> Array (Component st) -> Component st
caption = mkComponent "caption"

caption' :: forall st. Array (Component st) -> Component st
caption' = caption []

cite :: forall st. Array (Props st) -> Array (Component st) -> Component st
cite = mkComponent "cite"

cite' :: forall st. Array (Component st) -> Component st
cite' = cite []

code :: forall st. Array (Props st) -> Array (Component st) -> Component st
code = mkComponent "code"

code' :: forall st. Array (Component st) -> Component st
code' = code []

col :: forall st. Array (Props st) -> Array (Component st) -> Component st
col = mkComponent "col"

col' :: forall st. Array (Component st) -> Component st
col' = col []

colgroup :: forall st. Array (Props st) -> Array (Component st) -> Component st
colgroup = mkComponent "colgroup"

colgroup' :: forall st. Array (Component st) -> Component st
colgroup' = colgroup []

_data :: forall st. Array (Props st) -> Array (Component st) -> Component st
_data = mkComponent "data"

_data' :: forall st. Array (Component st) -> Component st
_data' = _data []

datalist :: forall st. Array (Props st) -> Array (Component st) -> Component st
datalist = mkComponent "datalist"

datalist' :: forall st. Array (Component st) -> Component st
datalist' = datalist []

dd :: forall st. Array (Props st) -> Array (Component st) -> Component st
dd = mkComponent "dd"

dd' :: forall st. Array (Component st) -> Component st
dd' = dd []

del :: forall st. Array (Props st) -> Array (Component st) -> Component st
del = mkComponent "del"

del' :: forall st. Array (Component st) -> Component st
del' = del []

details :: forall st. Array (Props st) -> Array (Component st) -> Component st
details = mkComponent "details"

details' :: forall st. Array (Component st) -> Component st
details' = details []

dfn :: forall st. Array (Props st) -> Array (Component st) -> Component st
dfn = mkComponent "dfn"

dfn' :: forall st. Array (Component st) -> Component st
dfn' = dfn []

dialog :: forall st. Array (Props st) -> Array (Component st) -> Component st
dialog = mkComponent "dialog"

dialog' :: forall st. Array (Component st) -> Component st
dialog' = dialog []

div :: forall st. Array (Props st) -> Array (Component st) -> Component st
div = mkComponent "div"

div' :: forall st. Array (Component st) -> Component st
div' = div []

dl :: forall st. Array (Props st) -> Array (Component st) -> Component st
dl = mkComponent "dl"

dl' :: forall st. Array (Component st) -> Component st
dl' = dl []

dt :: forall st. Array (Props st) -> Array (Component st) -> Component st
dt = mkComponent "dt"

dt' :: forall st. Array (Component st) -> Component st
dt' = dt []

em :: forall st. Array (Props st) -> Array (Component st) -> Component st
em = mkComponent "em"

em' :: forall st. Array (Component st) -> Component st
em' = em []

embed :: forall st. Array (Props st) -> Array (Component st) -> Component st
embed = mkComponent "embed"

embed' :: forall st. Array (Component st) -> Component st
embed' = embed []

fieldset :: forall st. Array (Props st) -> Array (Component st) -> Component st
fieldset = mkComponent "fieldset"

fieldset' :: forall st. Array (Component st) -> Component st
fieldset' = fieldset []

figcaption :: forall st. Array (Props st) -> Array (Component st) -> Component st
figcaption = mkComponent "figcaption"

figcaption' :: forall st. Array (Component st) -> Component st
figcaption' = figcaption []

figure :: forall st. Array (Props st) -> Array (Component st) -> Component st
figure = mkComponent "figure"

figure' :: forall st. Array (Component st) -> Component st
figure' = figure []

footer :: forall st. Array (Props st) -> Array (Component st) -> Component st
footer = mkComponent "footer"

footer' :: forall st. Array (Component st) -> Component st
footer' = footer []

form :: forall st. Array (Props st) -> Array (Component st) -> Component st
form = mkComponent "form"

form' :: forall st. Array (Component st) -> Component st
form' = form []

h1 :: forall st. Array (Props st) -> Array (Component st) -> Component st
h1 = mkComponent "h1"

h1' :: forall st. Array (Component st) -> Component st
h1' = h1 []

h2 :: forall st. Array (Props st) -> Array (Component st) -> Component st
h2 = mkComponent "h2"

h2' :: forall st. Array (Component st) -> Component st
h2' = h2 []

h3 :: forall st. Array (Props st) -> Array (Component st) -> Component st
h3 = mkComponent "h3"

h3' :: forall st. Array (Component st) -> Component st
h3' = h3 []

h4 :: forall st. Array (Props st) -> Array (Component st) -> Component st
h4 = mkComponent "h4"

h4' :: forall st. Array (Component st) -> Component st
h4' = h4 []

h5 :: forall st. Array (Props st) -> Array (Component st) -> Component st
h5 = mkComponent "h5"

h5' :: forall st. Array (Component st) -> Component st
h5' = h5 []

h6 :: forall st. Array (Props st) -> Array (Component st) -> Component st
h6 = mkComponent "h6"

h6' :: forall st. Array (Component st) -> Component st
h6' = h6 []

head :: forall st. Array (Props st) -> Array (Component st) -> Component st
head = mkComponent "head"

head' :: forall st. Array (Component st) -> Component st
head' = head []

header :: forall st. Array (Props st) -> Array (Component st) -> Component st
header = mkComponent "header"

header' :: forall st. Array (Component st) -> Component st
header' = header []

hr :: forall st. Array (Props st) -> Array (Component st) -> Component st
hr = mkComponent "hr"

hr' :: forall st. Array (Component st) -> Component st
hr' = hr []

html :: forall st. Array (Props st) -> Array (Component st) -> Component st
html = mkComponent "html"

html' :: forall st. Array (Component st) -> Component st
html' = html []

i :: forall st. Array (Props st) -> Array (Component st) -> Component st
i = mkComponent "i"

i' :: forall st. Array (Component st) -> Component st
i' = i []

iframe :: forall st. Array (Props st) -> Array (Component st) -> Component st
iframe = mkComponent "iframe"

iframe' :: forall st. Array (Component st) -> Component st
iframe' = iframe []

img :: forall st. Array (Props st) -> Array (Component st) -> Component st
img = mkComponent "img"

img' :: forall st. Array (Component st) -> Component st
img' = img []

input :: forall st. Array (Props st) -> Array (Component st) -> Component st
input = mkComponent "input"

input' :: forall st. Array (Component st) -> Component st
input' = input []

ins :: forall st. Array (Props st) -> Array (Component st) -> Component st
ins = mkComponent "ins"

ins' :: forall st. Array (Component st) -> Component st
ins' = ins []

kbd :: forall st. Array (Props st) -> Array (Component st) -> Component st
kbd = mkComponent "kbd"

kbd' :: forall st. Array (Component st) -> Component st
kbd' = kbd []

keygen :: forall st. Array (Props st) -> Array (Component st) -> Component st
keygen = mkComponent "keygen"

keygen' :: forall st. Array (Component st) -> Component st
keygen' = keygen []

label :: forall st. Array (Props st) -> Array (Component st) -> Component st
label = mkComponent "label"

label' :: forall st. Array (Component st) -> Component st
label' = label []

legend :: forall st. Array (Props st) -> Array (Component st) -> Component st
legend = mkComponent "legend"

legend' :: forall st. Array (Component st) -> Component st
legend' = legend []

li :: forall st. Array (Props st) -> Array (Component st) -> Component st
li = mkComponent "li"

li' :: forall st. Array (Component st) -> Component st
li' = li []

link :: forall st. Array (Props st) -> Array (Component st) -> Component st
link = mkComponent "link"

link' :: forall st. Array (Component st) -> Component st
link' = body []

main :: forall st. Array (Props st) -> Array (Component st) -> Component st
main = mkComponent "main"

main' :: forall st. Array (Component st) -> Component st
main' = main []

map :: forall st. Array (Props st) -> Array (Component st) -> Component st
map = mkComponent "map"

map' :: forall st. Array (Component st) -> Component st
map' = map []

mark :: forall st. Array (Props st) -> Array (Component st) -> Component st
mark = mkComponent "mark"

mark' :: forall st. Array (Component st) -> Component st
mark' = mark []

menu :: forall st. Array (Props st) -> Array (Component st) -> Component st
menu = mkComponent "menu"

menu' :: forall st. Array (Component st) -> Component st
menu' = menu []

menuitem :: forall st. Array (Props st) -> Array (Component st) -> Component st
menuitem = mkComponent "menuitem"

menuitem' :: forall st. Array (Component st) -> Component st
menuitem' = menuitem []

meta :: forall st. Array (Props st) -> Array (Component st) -> Component st
meta = mkComponent "meta"

meta' :: forall st. Array (Component st) -> Component st
meta' = meta []

meter :: forall st. Array (Props st) -> Array (Component st) -> Component st
meter = mkComponent "meter"

meter' :: forall st. Array (Component st) -> Component st
meter' = meter []

nav :: forall st. Array (Props st) -> Array (Component st) -> Component st
nav = mkComponent "nav"

nav' :: forall st. Array (Component st) -> Component st
nav' = nav []

noscript :: forall st. Array (Props st) -> Array (Component st) -> Component st
noscript = mkComponent "noscript"

noscript' :: forall st. Array (Component st) -> Component st
noscript' = noscript []

object :: forall st. Array (Props st) -> Array (Component st) -> Component st
object = mkComponent "object"

object' :: forall st. Array (Component st) -> Component st
object' = object []

ol :: forall st. Array (Props st) -> Array (Component st) -> Component st
ol = mkComponent "ol"

ol' :: forall st. Array (Component st) -> Component st
ol' = ol []

optgroup :: forall st. Array (Props st) -> Array (Component st) -> Component st
optgroup = mkComponent "optgroup"

optgroup' :: forall st. Array (Component st) -> Component st
optgroup' = optgroup []

option :: forall st. Array (Props st) -> Array (Component st) -> Component st
option = mkComponent "option"

option' :: forall st. Array (Component st) -> Component st
option' = option []

output :: forall st. Array (Props st) -> Array (Component st) -> Component st
output = mkComponent "output"

output' :: forall st. Array (Component st) -> Component st
output' = output []

p :: forall st. Array (Props st) -> Array (Component st) -> Component st
p = mkComponent "p"

p' :: forall st. Array (Component st) -> Component st
p' = p []

param :: forall st. Array (Props st) -> Array (Component st) -> Component st
param = mkComponent "param"

param' :: forall st. Array (Component st) -> Component st
param' = param []

picture :: forall st. Array (Props st) -> Array (Component st) -> Component st
picture = mkComponent "picture"

picture' :: forall st. Array (Component st) -> Component st
picture' = picture []

pre :: forall st. Array (Props st) -> Array (Component st) -> Component st
pre = mkComponent "pre"

pre' :: forall st. Array (Component st) -> Component st
pre' = pre []

progress :: forall st. Array (Props st) -> Array (Component st) -> Component st
progress = mkComponent "progress"

progress' :: forall st. Array (Component st) -> Component st
progress' = progress []

q :: forall st. Array (Props st) -> Array (Component st) -> Component st
q = mkComponent "q"

q' :: forall st. Array (Component st) -> Component st
q' = q []

rp :: forall st. Array (Props st) -> Array (Component st) -> Component st
rp = mkComponent "rp"

rp' :: forall st. Array (Component st) -> Component st
rp' = rp []

rt :: forall st. Array (Props st) -> Array (Component st) -> Component st
rt = mkComponent "rt"

rt' :: forall st. Array (Component st) -> Component st
rt' = rt []

ruby :: forall st. Array (Props st) -> Array (Component st) -> Component st
ruby = mkComponent "ruby"

ruby' :: forall st. Array (Component st) -> Component st
ruby' = ruby []

s :: forall st. Array (Props st) -> Array (Component st) -> Component st
s = mkComponent "s"

s' :: forall st. Array (Component st) -> Component st
s' = s []

samp :: forall st. Array (Props st) -> Array (Component st) -> Component st
samp = mkComponent "samp"

samp' :: forall st. Array (Component st) -> Component st
samp' = samp []

script :: forall st. Array (Props st) -> Array (Component st) -> Component st
script = mkComponent "script"

script' :: forall st. Array (Component st) -> Component st
script' = script []

section :: forall st. Array (Props st) -> Array (Component st) -> Component st
section = mkComponent "section"

section' :: forall st. Array (Component st) -> Component st
section' = section []

select :: forall st. Array (Props st) -> Array (Component st) -> Component st
select = mkComponent "select"

select' :: forall st. Array (Component st) -> Component st
select' = select []

small :: forall st. Array (Props st) -> Array (Component st) -> Component st
small = mkComponent "small"

small' :: forall st. Array (Component st) -> Component st
small' = small []

source :: forall st. Array (Props st) -> Array (Component st) -> Component st
source = mkComponent "source"

source' :: forall st. Array (Component st) -> Component st
source' = source []

span :: forall st. Array (Props st) -> Array (Component st) -> Component st
span = mkComponent "span"

span' :: forall st. Array (Component st) -> Component st
span' = span []

strong :: forall st. Array (Props st) -> Array (Component st) -> Component st
strong = mkComponent "strong"

strong' :: forall st. Array (Component st) -> Component st
strong' = strong []

style :: forall st. Array (Props st) -> Array (Component st) -> Component st
style = mkComponent "style"

style' :: forall st. Array (Component st) -> Component st
style' = style []

sub :: forall st. Array (Props st) -> Array (Component st) -> Component st
sub = mkComponent "sub"

sub' :: forall st. Array (Component st) -> Component st
sub' = sub []

summary :: forall st. Array (Props st) -> Array (Component st) -> Component st
summary = mkComponent "summary"

summary' :: forall st. Array (Component st) -> Component st
summary' = summary []

sup :: forall st. Array (Props st) -> Array (Component st) -> Component st
sup = mkComponent "sup"

sup' :: forall st. Array (Component st) -> Component st
sup' = sup []

table :: forall st. Array (Props st) -> Array (Component st) -> Component st
table = mkComponent "table"

table' :: forall st. Array (Component st) -> Component st
table' = table []

tbody :: forall st. Array (Props st) -> Array (Component st) -> Component st
tbody = mkComponent "tbody"

tbody' :: forall st. Array (Component st) -> Component st
tbody' = tbody []

td :: forall st. Array (Props st) -> Array (Component st) -> Component st
td = mkComponent "td"

td' :: forall st. Array (Component st) -> Component st
td' = td []

textarea :: forall st. Array (Props st) -> Array (Component st) -> Component st
textarea = mkComponent "textarea"

textarea' :: forall st. Array (Component st) -> Component st
textarea' = textarea []

tfoot :: forall st. Array (Props st) -> Array (Component st) -> Component st
tfoot = mkComponent "tfoot"

tfoot' :: forall st. Array (Component st) -> Component st
tfoot' = tfoot []

th :: forall st. Array (Props st) -> Array (Component st) -> Component st
th = mkComponent "th"

th' :: forall st. Array (Component st) -> Component st
th' = th []

thead :: forall st. Array (Props st) -> Array (Component st) -> Component st
thead = mkComponent "thead"

thead' :: forall st. Array (Component st) -> Component st
thead' = thead []

time :: forall st. Array (Props st) -> Array (Component st) -> Component st
time = mkComponent "time"

time' :: forall st. Array (Component st) -> Component st
time' = time []

title :: forall st. Array (Props st) -> Array (Component st) -> Component st
title = mkComponent "title"

title' :: forall st. Array (Component st) -> Component st
title' = title []

tr :: forall st. Array (Props st) -> Array (Component st) -> Component st
tr = mkComponent "tr"

tr' :: forall st. Array (Component st) -> Component st
tr' = tr []

track :: forall st. Array (Props st) -> Array (Component st) -> Component st
track = mkComponent "track"

track' :: forall st. Array (Component st) -> Component st
track' = track []

u :: forall st. Array (Props st) -> Array (Component st) -> Component st
u = mkComponent "u"

u' :: forall st. Array (Component st) -> Component st
u' = u []

ul :: forall st. Array (Props st) -> Array (Component st) -> Component st
ul = mkComponent "ul"

ul' :: forall st. Array (Component st) -> Component st
ul' = ul []

var :: forall st. Array (Props st) -> Array (Component st) -> Component st
var = mkComponent "var"

var' :: forall st. Array (Component st) -> Component st
var' = var []

video :: forall st. Array (Props st) -> Array (Component st) -> Component st
video = mkComponent "video"

video' :: forall st. Array (Component st) -> Component st
video' = video []

wbr :: forall st. Array (Props st) -> Array (Component st) -> Component st
wbr = mkComponent "body"

wbr' :: forall st. Array (Component st) -> Component st
wbr' = wbr []
