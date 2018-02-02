module Refract.DOM where

import Refract

import Unsafe.Coerce (unsafeCoerce)
  
text :: ∀ eff st. String -> Component eff st
text str _ _ = unsafeCoerce str

a :: ∀ eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
a = mkComponent "a"

a' :: ∀ eff st. Array (Component eff st) -> Component eff st
a' = a []

abbr :: ∀ eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
abbr = mkComponent "abbr"

abbr' :: ∀ eff st. Array (Component eff st) -> Component eff st
abbr' = abbr []

address :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
address = mkComponent "address"

address' :: forall eff st. Array (Component eff st) -> Component eff st
address' = address []

area :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
area = mkComponent "area"

area' :: forall eff st. Array (Component eff st) -> Component eff st
area' = area []

article :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
article = mkComponent "article"

article' :: forall eff st. Array (Component eff st) -> Component eff st
article' = article []

aside :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
aside = mkComponent "aside"

aside' :: forall eff st. Array (Component eff st) -> Component eff st
aside' = aside []

audio :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
audio = mkComponent "audio"

audio' :: forall eff st. Array (Component eff st) -> Component eff st
audio' = audio []

b :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
b = mkComponent "b"

b' :: forall eff st. Array (Component eff st) -> Component eff st
b' = b []

base :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
base = mkComponent "base"

base' :: forall eff st. Array (Component eff st) -> Component eff st
base' = base []

bdi :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
bdi = mkComponent "bdi"

bdi' :: forall eff st. Array (Component eff st) -> Component eff st
bdi' = bdi []

bdo :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
bdo = mkComponent "bdo"

bdo' :: forall eff st. Array (Component eff st) -> Component eff st
bdo' = bdo []

big :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
big = mkComponent "big"

big' :: forall eff st. Array (Component eff st) -> Component eff st
big' = big []

blockquote :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
blockquote = mkComponent "blockquote"

blockquote' :: forall eff st. Array (Component eff st) -> Component eff st
blockquote' = blockquote []

body :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
body = mkComponent "body"

body' :: forall eff st. Array (Component eff st) -> Component eff st
body' = body []

br :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
br = mkComponent "br"

br' :: forall eff st. Array (Component eff st) -> Component eff st
br' = br []

button :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
button = mkComponent "button"

button' :: forall eff st. Array (Component eff st) -> Component eff st
button' = button []

canvas :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
canvas = mkComponent "canvas"

canvas' :: forall eff st. Array (Component eff st) -> Component eff st
canvas' = canvas []

caption :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
caption = mkComponent "caption"

caption' :: forall eff st. Array (Component eff st) -> Component eff st
caption' = caption []

cite :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
cite = mkComponent "cite"

cite' :: forall eff st. Array (Component eff st) -> Component eff st
cite' = cite []

code :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
code = mkComponent "code"

code' :: forall eff st. Array (Component eff st) -> Component eff st
code' = code []

col :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
col = mkComponent "col"

col' :: forall eff st. Array (Component eff st) -> Component eff st
col' = col []

colgroup :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
colgroup = mkComponent "colgroup"

colgroup' :: forall eff st. Array (Component eff st) -> Component eff st
colgroup' = colgroup []

_data :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
_data = mkComponent "data"

_data' :: forall eff st. Array (Component eff st) -> Component eff st
_data' = _data []

datalist :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
datalist = mkComponent "datalist"

datalist' :: forall eff st. Array (Component eff st) -> Component eff st
datalist' = datalist []

dd :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
dd = mkComponent "dd"

dd' :: forall eff st. Array (Component eff st) -> Component eff st
dd' = dd []

del :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
del = mkComponent "del"

del' :: forall eff st. Array (Component eff st) -> Component eff st
del' = del []

details :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
details = mkComponent "details"

details' :: forall eff st. Array (Component eff st) -> Component eff st
details' = details []

dfn :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
dfn = mkComponent "dfn"

dfn' :: forall eff st. Array (Component eff st) -> Component eff st
dfn' = dfn []

dialog :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
dialog = mkComponent "dialog"

dialog' :: forall eff st. Array (Component eff st) -> Component eff st
dialog' = dialog []

div :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
div = mkComponent "div"

div' :: forall eff st. Array (Component eff st) -> Component eff st
div' = div []

dl :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
dl = mkComponent "dl"

dl' :: forall eff st. Array (Component eff st) -> Component eff st
dl' = dl []

dt :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
dt = mkComponent "dt"

dt' :: forall eff st. Array (Component eff st) -> Component eff st
dt' = dt []

em :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
em = mkComponent "em"

em' :: forall eff st. Array (Component eff st) -> Component eff st
em' = em []

embed :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
embed = mkComponent "embed"

embed' :: forall eff st. Array (Component eff st) -> Component eff st
embed' = embed []

fieldset :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
fieldset = mkComponent "fieldset"

fieldset' :: forall eff st. Array (Component eff st) -> Component eff st
fieldset' = fieldset []

figcaption :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
figcaption = mkComponent "figcaption"

figcaption' :: forall eff st. Array (Component eff st) -> Component eff st
figcaption' = figcaption []

figure :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
figure = mkComponent "figure"

figure' :: forall eff st. Array (Component eff st) -> Component eff st
figure' = figure []

footer :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
footer = mkComponent "footer"

footer' :: forall eff st. Array (Component eff st) -> Component eff st
footer' = footer []

form :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
form = mkComponent "form"

form' :: forall eff st. Array (Component eff st) -> Component eff st
form' = form []

h1 :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
h1 = mkComponent "h1"

h1' :: forall eff st. Array (Component eff st) -> Component eff st
h1' = h1 []

h2 :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
h2 = mkComponent "h2"

h2' :: forall eff st. Array (Component eff st) -> Component eff st
h2' = h2 []

h3 :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
h3 = mkComponent "h3"

h3' :: forall eff st. Array (Component eff st) -> Component eff st
h3' = h3 []

h4 :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
h4 = mkComponent "h4"

h4' :: forall eff st. Array (Component eff st) -> Component eff st
h4' = h4 []

h5 :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
h5 = mkComponent "h5"

h5' :: forall eff st. Array (Component eff st) -> Component eff st
h5' = h5 []

h6 :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
h6 = mkComponent "h6"

h6' :: forall eff st. Array (Component eff st) -> Component eff st
h6' = h6 []

head :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
head = mkComponent "head"

head' :: forall eff st. Array (Component eff st) -> Component eff st
head' = head []

header :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
header = mkComponent "header"

header' :: forall eff st. Array (Component eff st) -> Component eff st
header' = header []

hr :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
hr = mkComponent "hr"

hr' :: forall eff st. Array (Component eff st) -> Component eff st
hr' = hr []

html :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
html = mkComponent "html"

html' :: forall eff st. Array (Component eff st) -> Component eff st
html' = html []

i :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
i = mkComponent "i"

i' :: forall eff st. Array (Component eff st) -> Component eff st
i' = i []

iframe :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
iframe = mkComponent "iframe"

iframe' :: forall eff st. Array (Component eff st) -> Component eff st
iframe' = iframe []

img :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
img = mkComponent "img"

img' :: forall eff st. Array (Component eff st) -> Component eff st
img' = img []

input :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
input = mkComponent "input"

input' :: forall eff st. Array (Component eff st) -> Component eff st
input' = input []

ins :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
ins = mkComponent "ins"

ins' :: forall eff st. Array (Component eff st) -> Component eff st
ins' = ins []

kbd :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
kbd = mkComponent "kbd"

kbd' :: forall eff st. Array (Component eff st) -> Component eff st
kbd' = kbd []

keygen :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
keygen = mkComponent "keygen"

keygen' :: forall eff st. Array (Component eff st) -> Component eff st
keygen' = keygen []

label :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
label = mkComponent "label"

label' :: forall eff st. Array (Component eff st) -> Component eff st
label' = label []

legend :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
legend = mkComponent "legend"

legend' :: forall eff st. Array (Component eff st) -> Component eff st
legend' = legend []

li :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
li = mkComponent "li"

li' :: forall eff st. Array (Component eff st) -> Component eff st
li' = li []

link :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
link = mkComponent "link"

link' :: forall eff st. Array (Component eff st) -> Component eff st
link' = body []

main :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
main = mkComponent "main"

main' :: forall eff st. Array (Component eff st) -> Component eff st
main' = main []

map :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
map = mkComponent "map"

map' :: forall eff st. Array (Component eff st) -> Component eff st
map' = map []

mark :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
mark = mkComponent "mark"

mark' :: forall eff st. Array (Component eff st) -> Component eff st
mark' = mark []

menu :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
menu = mkComponent "menu"

menu' :: forall eff st. Array (Component eff st) -> Component eff st
menu' = menu []

menuitem :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
menuitem = mkComponent "menuitem"

menuitem' :: forall eff st. Array (Component eff st) -> Component eff st
menuitem' = menuitem []

meta :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
meta = mkComponent "meta"

meta' :: forall eff st. Array (Component eff st) -> Component eff st
meta' = meta []

meter :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
meter = mkComponent "meter"

meter' :: forall eff st. Array (Component eff st) -> Component eff st
meter' = meter []

nav :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
nav = mkComponent "nav"

nav' :: forall eff st. Array (Component eff st) -> Component eff st
nav' = nav []

noscript :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
noscript = mkComponent "noscript"

noscript' :: forall eff st. Array (Component eff st) -> Component eff st
noscript' = noscript []

object :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
object = mkComponent "object"

object' :: forall eff st. Array (Component eff st) -> Component eff st
object' = object []

ol :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
ol = mkComponent "ol"

ol' :: forall eff st. Array (Component eff st) -> Component eff st
ol' = ol []

optgroup :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
optgroup = mkComponent "optgroup"

optgroup' :: forall eff st. Array (Component eff st) -> Component eff st
optgroup' = optgroup []

option :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
option = mkComponent "option"

option' :: forall eff st. Array (Component eff st) -> Component eff st
option' = option []

output :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
output = mkComponent "output"

output' :: forall eff st. Array (Component eff st) -> Component eff st
output' = output []

p :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
p = mkComponent "p"

p' :: forall eff st. Array (Component eff st) -> Component eff st
p' = p []

param :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
param = mkComponent "param"

param' :: forall eff st. Array (Component eff st) -> Component eff st
param' = param []

picture :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
picture = mkComponent "picture"

picture' :: forall eff st. Array (Component eff st) -> Component eff st
picture' = picture []

pre :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
pre = mkComponent "pre"

pre' :: forall eff st. Array (Component eff st) -> Component eff st
pre' = pre []

progress :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
progress = mkComponent "progress"

progress' :: forall eff st. Array (Component eff st) -> Component eff st
progress' = progress []

q :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
q = mkComponent "q"

q' :: forall eff st. Array (Component eff st) -> Component eff st
q' = q []

rp :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
rp = mkComponent "rp"

rp' :: forall eff st. Array (Component eff st) -> Component eff st
rp' = rp []

rt :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
rt = mkComponent "rt"

rt' :: forall eff st. Array (Component eff st) -> Component eff st
rt' = rt []

ruby :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
ruby = mkComponent "ruby"

ruby' :: forall eff st. Array (Component eff st) -> Component eff st
ruby' = ruby []

s :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
s = mkComponent "s"

s' :: forall eff st. Array (Component eff st) -> Component eff st
s' = s []

samp :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
samp = mkComponent "samp"

samp' :: forall eff st. Array (Component eff st) -> Component eff st
samp' = samp []

script :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
script = mkComponent "script"

script' :: forall eff st. Array (Component eff st) -> Component eff st
script' = script []

section :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
section = mkComponent "section"

section' :: forall eff st. Array (Component eff st) -> Component eff st
section' = section []

select :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
select = mkComponent "select"

select' :: forall eff st. Array (Component eff st) -> Component eff st
select' = select []

small :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
small = mkComponent "small"

small' :: forall eff st. Array (Component eff st) -> Component eff st
small' = small []

source :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
source = mkComponent "source"

source' :: forall eff st. Array (Component eff st) -> Component eff st
source' = source []

span :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
span = mkComponent "span"

span' :: forall eff st. Array (Component eff st) -> Component eff st
span' = span []

strong :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
strong = mkComponent "strong"

strong' :: forall eff st. Array (Component eff st) -> Component eff st
strong' = strong []

style :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
style = mkComponent "style"

style' :: forall eff st. Array (Component eff st) -> Component eff st
style' = style []

sub :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
sub = mkComponent "sub"

sub' :: forall eff st. Array (Component eff st) -> Component eff st
sub' = sub []

summary :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
summary = mkComponent "summary"

summary' :: forall eff st. Array (Component eff st) -> Component eff st
summary' = summary []

sup :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
sup = mkComponent "sup"

sup' :: forall eff st. Array (Component eff st) -> Component eff st
sup' = sup []

table :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
table = mkComponent "table"

table' :: forall eff st. Array (Component eff st) -> Component eff st
table' = table []

tbody :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
tbody = mkComponent "tbody"

tbody' :: forall eff st. Array (Component eff st) -> Component eff st
tbody' = tbody []

td :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
td = mkComponent "td"

td' :: forall eff st. Array (Component eff st) -> Component eff st
td' = td []

textarea :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
textarea = mkComponent "textarea"

textarea' :: forall eff st. Array (Component eff st) -> Component eff st
textarea' = textarea []

tfoot :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
tfoot = mkComponent "tfoot"

tfoot' :: forall eff st. Array (Component eff st) -> Component eff st
tfoot' = tfoot []

th :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
th = mkComponent "th"

th' :: forall eff st. Array (Component eff st) -> Component eff st
th' = th []

thead :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
thead = mkComponent "thead"

thead' :: forall eff st. Array (Component eff st) -> Component eff st
thead' = thead []

time :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
time = mkComponent "time"

time' :: forall eff st. Array (Component eff st) -> Component eff st
time' = time []

title :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
title = mkComponent "title"

title' :: forall eff st. Array (Component eff st) -> Component eff st
title' = title []

tr :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
tr = mkComponent "tr"

tr' :: forall eff st. Array (Component eff st) -> Component eff st
tr' = tr []

track :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
track = mkComponent "track"

track' :: forall eff st. Array (Component eff st) -> Component eff st
track' = track []

u :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
u = mkComponent "u"

u' :: forall eff st. Array (Component eff st) -> Component eff st
u' = u []

ul :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
ul = mkComponent "ul"

ul' :: forall eff st. Array (Component eff st) -> Component eff st
ul' = ul []

var :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
var = mkComponent "var"

var' :: forall eff st. Array (Component eff st) -> Component eff st
var' = var []

video :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
video = mkComponent "video"

video' :: forall eff st. Array (Component eff st) -> Component eff st
video' = video []

wbr :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
wbr = mkComponent "body"

wbr' :: forall eff st. Array (Component eff st) -> Component eff st
wbr' = wbr []
