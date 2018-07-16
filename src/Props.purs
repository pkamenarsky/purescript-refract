module Props where

import Prelude

import Data.Int (round)
import React.SyntheticEvent as Event
import React.DOM.Props as P
import Refract (Effect, Props, (○))

aria :: ∀ ariaAttrs st s. { | ariaAttrs } -> Props st s
aria v _ = P.aria v

_data :: ∀ dataAttrs st s. { | dataAttrs } -> Props st s
_data v _ = P._data v

style :: ∀ style st s. { | style } -> Props st s
style v _ = P.style v

dangerouslySetInnerHTML :: ∀ st s. { __html :: String } -> Props st s
dangerouslySetInnerHTML v _ = P.dangerouslySetInnerHTML v

accept :: ∀ st s. String -> Props st s
accept v _ = P.accept v

acceptCharset :: ∀ st s. String -> Props st s
acceptCharset v _ = P.acceptCharset v

accessKey :: ∀ st s. String -> Props st s
accessKey v _ = P.accessKey v

action :: ∀ st s. String -> Props st s
action v _ = P.action v

allowFullScreen :: ∀ st s. Boolean -> Props st s
allowFullScreen v _ = P.allowFullScreen v

allowTransparency :: ∀ st s. Boolean -> Props st s
allowTransparency v _ = P.allowTransparency v

alt :: ∀ st s. String -> Props st s
alt v _ = P.alt v

async :: ∀ st s. Boolean -> Props st s
async v _ = P.async v

autoComplete :: ∀ st s. String -> Props st s
autoComplete v _ = P.autoComplete v

autoFocus :: ∀ st s. Boolean -> Props st s
autoFocus v _ = P.autoFocus v

autoPlay :: ∀ st s. Boolean -> Props st s
autoPlay v _ = P.autoPlay v

capture :: ∀ st s. Boolean -> Props st s
capture v _ = P.capture v

cellPadding :: ∀ st s. String -> Props st s
cellPadding v _ = P.cellPadding v

cellSpacing :: ∀ st s. String -> Props st s
cellSpacing v _ = P.cellSpacing v

charSet :: ∀ st s. String -> Props st s
charSet v _ = P.charSet v

challenge :: ∀ st s. String -> Props st s
challenge v _ = P.challenge v

checked :: ∀ st s. Boolean -> Props st s
checked v _ = P.checked v

cite :: ∀ st s. String -> Props st s
cite v _ = P.cite v

classID :: ∀ st s. String -> Props st s
classID v _ = P.classID v

className :: ∀ st s. String -> Props st s
className v _ = P.className v

cols :: ∀ st s. Int -> Props st s
cols v _ = P.cols v

colSpan :: ∀ st s. Int -> Props st s
colSpan v _ = P.colSpan v

content :: ∀ st s. String -> Props st s
content v _ = P.content v

contentEditable :: ∀ st s. Boolean -> Props st s
contentEditable v _ = P.contentEditable v

contextMenu :: ∀ st s. String -> Props st s
contextMenu v _ = P.contextMenu v

controls :: ∀ st s. Boolean -> Props st s
controls v _ = P.controls v

coords :: ∀ st s. String -> Props st s
coords v _ = P.coords v

crossOrigin :: ∀ st s. String -> Props st s
crossOrigin v _ = P.crossOrigin v

dateTime :: ∀ st s. String -> Props st s
dateTime v _ = P.dateTime v

default :: ∀ st s. Boolean -> Props st s
default v _ = P.default v

defaultChecked :: ∀ st s. Boolean -> Props st s
defaultChecked v _ = P.defaultChecked v

defaultValue :: ∀ st s. String -> Props st s
defaultValue v _ = P.defaultValue v

defer :: ∀ st s. Boolean -> Props st s
defer v _ = P.defer v

dir :: ∀ st s. String -> Props st s
dir v _ = P.dir v

disabled :: ∀ st s. Boolean -> Props st s
disabled v _ = P.disabled v

download :: ∀ st s. String -> Props st s
download v _ = P.download v

draggable :: ∀ st s. Boolean -> Props st s
draggable v _ = P.draggable v

encType :: ∀ st s. String -> Props st s
encType v _ = P.encType v

form :: ∀ st s. String -> Props st s
form v _ = P.form v

formAction :: ∀ st s. String -> Props st s
formAction v _ = P.formAction v

formEncType :: ∀ st s. String -> Props st s
formEncType v _ = P.formEncType v

formMethod :: ∀ st s. String -> Props st s
formMethod v _ = P.formMethod v

formNoValidate :: ∀ st s. Boolean -> Props st s
formNoValidate v _ = P.formNoValidate v

formTarget :: ∀ st s. String -> Props st s
formTarget v _ = P.formTarget v

frameBorder :: ∀ st s. String -> Props st s
frameBorder v _ = P.frameBorder v

headers :: ∀ st s. String -> Props st s
headers v _ = P.headers v

height :: ∀ st s. String -> Props st s
height v _ = P.height v

hidden :: ∀ st s. Boolean -> Props st s
hidden v _ = P.hidden v

high :: ∀ st s. String -> Props st s
high v _ = P.high v

href :: ∀ st s. String -> Props st s
href v _ = P.href v

hrefLang :: ∀ st s. String -> Props st s
hrefLang v _ = P.hrefLang v

htmlFor :: ∀ st s. String -> Props st s
htmlFor v _ = P.htmlFor v

httpEquiv :: ∀ st s. String -> Props st s
httpEquiv v _ = P.httpEquiv v

icon :: ∀ st s. String -> Props st s
icon v _ = P.icon v

_id :: ∀ st s. String -> Props st s
_id v _ = P._id v

inputMode :: ∀ st s. String -> Props st s
inputMode v _ = P.inputMode v

integrity :: ∀ st s. String -> Props st s
integrity v _ = P.integrity v

is :: ∀ st s. String -> Props st s
is v _ = P.is v

key :: ∀ st s. String -> Props st s
key v _ = P.key v

keyParams :: ∀ st s. String -> Props st s
keyParams v _ = P.keyParams v

keyType :: ∀ st s. String -> Props st s
keyType v _ = P.keyType v

kind :: ∀ st s. String -> Props st s
kind v _ = P.kind v

label :: ∀ st s. String -> Props st s
label v _ = P.label v

lang :: ∀ st s. String -> Props st s
lang v _ = P.lang v

list :: ∀ st s. String -> Props st s
list v _ = P.list v

loop :: ∀ st s. Boolean -> Props st s
loop v _ = P.loop v

low :: ∀ st s. String -> Props st s
low v _ = P.low v

manifest :: ∀ st s. String -> Props st s
manifest v _ = P.manifest v

marginHeight :: ∀ st s. String -> Props st s
marginHeight v _ = P.marginHeight v

marginWidth :: ∀ st s. String -> Props st s
marginWidth v _ = P.marginWidth v

max :: ∀ st s. String -> Props st s
max v _ = P.max v

maxLength :: ∀ st s. String -> Props st s
maxLength v _ = P.maxLength v

media :: ∀ st s. String -> Props st s
media v _ = P.media v

mediaGroup :: ∀ st s. String -> Props st s
mediaGroup v _ = P.mediaGroup v

method :: ∀ st s. String -> Props st s
method v _ = P.method v

min :: ∀ st s. String -> Props st s
min v _ = P.min v

minLength :: ∀ st s. String -> Props st s
minLength v _ = P.minLength v

multiple :: ∀ st s. Boolean -> Props st s
multiple v _ = P.multiple v

muted :: ∀ st s. Boolean -> Props st s
muted v _ = P.muted v

name :: ∀ st s. String -> Props st s
name v _ = P.name v

nonce :: ∀ st s. String -> Props st s
nonce v _ = P.nonce v

noValidate :: ∀ st s. Boolean -> Props st s
noValidate v _ = P.noValidate v

open :: ∀ st s. Boolean -> Props st s
open v _ = P.open v

optimum :: ∀ st s. String -> Props st s
optimum v _ = P.optimum v

pattern :: ∀ st s. String -> Props st s
pattern v _ = P.pattern v

placeholder :: ∀ st s. String -> Props st s
placeholder v _ = P.placeholder v

poster :: ∀ st s. String -> Props st s
poster v _ = P.poster v

preload :: ∀ st s. String -> Props st s
preload v _ = P.preload v

profile :: ∀ st s. String -> Props st s
profile v _ = P.profile v

radioGroup :: ∀ st s. String -> Props st s
radioGroup v _ = P.radioGroup v

readOnly :: ∀ st s. Boolean -> Props st s
readOnly v _ = P.readOnly v

rel :: ∀ st s. String -> Props st s
rel v _ = P.rel v

required :: ∀ st s. Boolean -> Props st s
required v _ = P.required v

reversed :: ∀ st s. Boolean -> Props st s
reversed v _ = P.reversed v

role :: ∀ st s. String -> Props st s
role v _ = P.role v

rows :: ∀ st s. Int -> Props st s
rows v _ = P.rows v

rowSpan :: ∀ st s. Int -> Props st s
rowSpan v _ = P.rowSpan v

sandbox :: ∀ st s. String -> Props st s
sandbox v _ = P.sandbox v

scope :: ∀ st s. String -> Props st s
scope v _ = P.scope v

scoped :: ∀ st s. Boolean -> Props st s
scoped v _ = P.scoped v

scrolling :: ∀ st s. String -> Props st s
scrolling v _ = P.scrolling v

seamless :: ∀ st s. Boolean -> Props st s
seamless v _ = P.seamless v

selected :: ∀ st s. Boolean -> Props st s
selected v _ = P.selected v

shape :: ∀ st s. String -> Props st s
shape v _ = P.shape v

size :: ∀ st s. Int -> Props st s
size v _ = P.size v

sizes :: ∀ st s. String -> Props st s
sizes v _ = P.sizes v

span :: ∀ st s. Int -> Props st s
span v _ = P.span v

spellCheck :: ∀ st s. Boolean -> Props st s
spellCheck v _ = P.spellCheck v

src :: ∀ st s. String -> Props st s
src v _ = P.src v

srcDoc :: ∀ st s. String -> Props st s
srcDoc v _ = P.srcDoc v

srcLang :: ∀ st s. String -> Props st s
srcLang v _ = P.srcLang v

srcSet :: ∀ st s. String -> Props st s
srcSet v _ = P.srcSet v

start :: ∀ st s. Int -> Props st s
start v _ = P.start v

step :: ∀ st s. String -> Props st s
step v _ = P.step v

summary :: ∀ st s. String -> Props st s
summary v _ = P.summary v

tabIndex :: ∀ st s. Int -> Props st s
tabIndex v _ = P.tabIndex v

target :: ∀ st s. String -> Props st s
target v _ = P.target v

title :: ∀ st s. String -> Props st s
title v _ = P.title v

_type :: ∀ st s. String -> Props st s
_type v _ = P._type v

useMap :: ∀ st s. String -> Props st s
useMap v _ = P.useMap v

value :: ∀ st s. String -> Props st s
value v _ = P.value v

width :: ∀ st s. String -> Props st s
width v _ = P.width v

wmode :: ∀ st s. String -> Props st s
wmode v _ = P.wmode v

wrap :: ∀ st s. String -> Props st s
wrap v _ = P.wrap v

-- RDFa Attributes
about :: ∀ st s. String -> Props st s
about v _ = P.about v

datatype :: ∀ st s. String -> Props st s
datatype v _ = P.datatype v

inlist :: ∀ st s. String -> Props st s
inlist v _ = P.inlist v

prefix :: ∀ st s. String -> Props st s
prefix v _ = P.prefix v

property :: ∀ st s. String -> Props st s
property v _ = P.property v

resource :: ∀ st s. String -> Props st s
resource v _ = P.resource v

typeof :: ∀ st s. String -> Props st s
typeof v _ = P.typeof v

vocab :: ∀ st s. String -> Props st s
vocab v _ = P.vocab v

-- Non-standard Attributes
autoCapitalize :: ∀ st s. String -> Props st s
autoCapitalize v _ = P.autoCapitalize v

autoCorrect :: ∀ st s. String -> Props st s
autoCorrect v _ = P.autoCorrect v

autoSave :: ∀ st s. String -> Props st s
autoSave v _ = P.autoSave v

color :: ∀ st s. String -> Props st s
color v _ = P.color v

itemProp :: ∀ st s. String -> Props st s
itemProp v _ = P.itemProp v

itemScope :: ∀ st s. Boolean -> Props st s
itemScope v _ = P.itemScope v

itemType :: ∀ st s. String -> Props st s
itemType v _ = P.itemType v

itemID :: ∀ st s. String -> Props st s
itemID v _ = P.itemID v

itemRef :: ∀ st s. String -> Props st s
itemRef v _ = P.itemRef v

results :: ∀ st s. Int -> Props st s
results v _ = P.results v

security :: ∀ st s. String -> Props st s
security v _ = P.security v

unselectable :: ∀ st s. Boolean -> Props st s
unselectable v _ = P.unselectable v

--------------------------------------------------------------------------------

onAnimationStart :: ∀ st s.
  (Event.SyntheticAnimationEvent -> Effect st s Unit) -> Props st s
onAnimationStart f effect = P.onAnimationStart (effect ○ f)

onAnimationEnd :: ∀ st s.
  (Event.SyntheticAnimationEvent -> Effect st s Unit) -> Props st s
onAnimationEnd f effect = P.onAnimationEnd (effect ○ f)

onAnimationIteration :: ∀ st s.
  (Event.SyntheticAnimationEvent -> Effect st s Unit) -> Props st s
onAnimationIteration f effect = P.onAnimationIteration (effect ○ f)

onTransitionEnd :: ∀ st s.
  (Event.SyntheticTransitionEvent -> Effect st s Unit) -> Props st s
onTransitionEnd f effect = P.onTransitionEnd (effect ○ f)

onLoad :: ∀ st s.
  (Event.SyntheticEvent -> Effect st s Unit) -> Props st s
onLoad f effect = P.onLoad (effect ○ f)

onCopy :: ∀ st s.
  (Event.SyntheticClipboardEvent -> Effect st s Unit) -> Props st s
onCopy f effect = P.onCopy (effect ○ f)

onCut :: ∀ st s.
  (Event.SyntheticClipboardEvent -> Effect st s Unit) -> Props st s
onCut f effect = P.onCut (effect ○ f)

onPaste :: ∀ st s.
  (Event.SyntheticClipboardEvent -> Effect st s Unit) -> Props st s
onPaste f effect = P.onPaste (effect ○ f)

onKeyDown :: ∀ st s.
  (Event.SyntheticKeyboardEvent -> Effect st s Unit) -> Props st s
onKeyDown f effect = P.onKeyDown (effect ○ f)

onKeyPress :: ∀ st s.
  (Event.SyntheticKeyboardEvent -> Effect st s Unit) -> Props st s
onKeyPress f effect = P.onKeyPress (effect ○ f)

onKeyUp :: ∀ st s.
  (Event.SyntheticKeyboardEvent -> Effect st s Unit) -> Props st s
onKeyUp f effect = P.onKeyUp (effect ○ f)

onFocus :: ∀ st s.
  (Event.SyntheticFocusEvent -> Effect st s Unit) -> Props st s
onFocus f effect = P.onFocus (effect ○ f)

onBlur :: ∀ st s.
  (Event.SyntheticFocusEvent -> Effect st s Unit) -> Props st s
onBlur f effect = P.onBlur (effect ○ f)

onChange :: ∀ st s.
  (Event.SyntheticInputEvent -> Effect st s Unit) -> Props st s
onChange f effect = P.onChange (effect ○ f)

onInput :: ∀ st s.
  (Event.SyntheticInputEvent -> Effect st s Unit) -> Props st s
onInput f effect = P.onInput (effect ○ f)

onInvalid :: ∀ st s.
  (Event.SyntheticInputEvent -> Effect st s Unit) -> Props st s
onInvalid f effect = P.onInvalid (effect ○ f)

onSubmit :: ∀ st s.
  (Event.SyntheticInputEvent -> Effect st s Unit) -> Props st s
onSubmit f effect = P.onSubmit (effect ○ f)

onClick :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onClick f effect = P.onClick (effect ○ f)

onDoubleClick :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onDoubleClick f effect = P.onDoubleClick (effect ○ f)

onDrag :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onDrag f effect = P.onDrag (effect ○ f)

onDragEnd :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onDragEnd f effect = P.onDragEnd (effect ○ f)

onDragEnter :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onDragEnter f effect = P.onDragEnter (effect ○ f)

onDragExit :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onDragExit f effect = P.onDragExit (effect ○ f)

onDragLeave :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onDragLeave f effect = P.onDragLeave (effect ○ f)

onDragOver :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onDragOver f effect = P.onDragOver (effect ○ f)

onDragStart :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onDragStart f effect = P.onDragStart (effect ○ f)

onDrop :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onDrop f effect = P.onDrop (effect ○ f)

onMouseDown :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onMouseDown f effect = P.onMouseDown (effect ○ f)

onMouseEnter :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onMouseEnter f effect = P.onMouseEnter (effect ○ f)

onMouseLeave :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onMouseLeave f effect = P.onMouseLeave (effect ○ f)

onMouseMove :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onMouseMove f effect = P.onMouseMove (effect ○ f)

onMouseOut :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onMouseOut f effect = P.onMouseOut (effect ○ f)

onMouseOver :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onMouseOver f effect = P.onMouseOver (effect ○ f)

onMouseUp :: ∀ st s.
  (Event.SyntheticMouseEvent -> Effect st s Unit) -> Props st s
onMouseUp f effect = P.onMouseUp (effect ○ f)

onTouchCancel :: ∀ st s.
  (Event.SyntheticTouchEvent -> Effect st s Unit) -> Props st s
onTouchCancel f effect = P.onTouchCancel (effect ○ f)

onTouchEnd :: ∀ st s.
  (Event.SyntheticTouchEvent -> Effect st s Unit) -> Props st s
onTouchEnd f effect = P.onTouchEnd (effect ○ f)

onTouchMove :: ∀ st s.
  (Event.SyntheticTouchEvent -> Effect st s Unit) -> Props st s
onTouchMove f effect = P.onTouchMove (effect ○ f)

onTouchStart :: ∀ st s.
  (Event.SyntheticTouchEvent -> Effect st s Unit) -> Props st s
onTouchStart f effect = P.onTouchStart (effect ○ f)

onScroll :: ∀ st s.
  (Event.SyntheticUIEvent -> Effect st s Unit) -> Props st s
onScroll f effect = P.onScroll (effect ○ f)

onWheel :: ∀ st s.
  (Event.SyntheticWheelEvent -> Effect st s Unit) -> Props st s
onWheel f effect = P.onWheel (effect ○ f)

--------------------------------------------------------------------------------

suppressContentEditableWarning :: ∀ st s. Boolean -> Props st s
suppressContentEditableWarning v _ = P.suppressContentEditableWarning v

-- SVG attributes
x :: ∀ st s. Int -> Props st s
x v _ = P.x v

y :: ∀ st s. Int -> Props st s
y v _ = P.y v

cx :: ∀ st s. Int -> Props st s
cx v _ = P.cx v

cy :: ∀ st s. Int -> Props st s
cy v _ = P.cy v

r :: ∀ st s. Int -> Props st s
r v _ = P.r v

fill :: ∀ st s. String -> Props st s
fill v _ = P.fill v

opacity :: ∀ st s. Int -> Props st s
opacity v _ = P.opacity v

fillOpacity :: ∀ st s. Int -> Props st s
fillOpacity v _ = P.fillOpacity v

stroke :: ∀ st s. String -> Props st s
stroke v _ = P.stroke v

strokeWidth :: ∀ st s. Int -> Props st s
strokeWidth v _ = P.strokeWidth v

points :: ∀ st s. String -> Props st s
points v _ = P.points v

d :: ∀ st s. String -> Props st s
d v _ = P.d v

viewBox :: ∀ st s. String -> Props st s
viewBox v _ = P.viewBox v

-- --------------------------------------------------------------------------------

onEnter :: ∀ st s. Effect st s Unit -> Props st s
onEnter f effect = P.onKeyDown \e -> do
  keyCode <- Event.keyCode e
  if round keyCode == 13 then effect f else pure unit

onEscape :: ∀ st s. Effect st s Unit -> Props st s
onEscape f effect = P.onKeyDown \e -> do
  keyCode <- Event.keyCode e
  if round keyCode == 27 then effect f else pure unit
