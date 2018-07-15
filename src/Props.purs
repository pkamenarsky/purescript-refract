module Props where

import Prelude

import Data.Int (round)
import React.SyntheticEvent as Event
import React.DOM.Props as P
import Refract (Effect, Props, (○))

aria :: ∀ ariaAttrs st. { | ariaAttrs } -> Props st
aria v _ = P.aria v

_data :: ∀ dataAttrs st. { | dataAttrs } -> Props st
_data v _ = P._data v

style :: ∀ style st. { | style } -> Props st
style v _ = P.style v

dangerouslySetInnerHTML :: ∀ st. { __html :: String } -> Props st
dangerouslySetInnerHTML v _ = P.dangerouslySetInnerHTML v

accept :: ∀ st. String -> Props st
accept v _ = P.accept v

acceptCharset :: ∀ st. String -> Props st
acceptCharset v _ = P.acceptCharset v

accessKey :: ∀ st. String -> Props st
accessKey v _ = P.accessKey v

action :: ∀ st. String -> Props st
action v _ = P.action v

allowFullScreen :: ∀ st. Boolean -> Props st
allowFullScreen v _ = P.allowFullScreen v

allowTransparency :: ∀ st. Boolean -> Props st
allowTransparency v _ = P.allowTransparency v

alt :: ∀ st. String -> Props st
alt v _ = P.alt v

async :: ∀ st. Boolean -> Props st
async v _ = P.async v

autoComplete :: ∀ st. String -> Props st
autoComplete v _ = P.autoComplete v

autoFocus :: ∀ st. Boolean -> Props st
autoFocus v _ = P.autoFocus v

autoPlay :: ∀ st. Boolean -> Props st
autoPlay v _ = P.autoPlay v

capture :: ∀ st. Boolean -> Props st
capture v _ = P.capture v

cellPadding :: ∀ st. String -> Props st
cellPadding v _ = P.cellPadding v

cellSpacing :: ∀ st. String -> Props st
cellSpacing v _ = P.cellSpacing v

charSet :: ∀ st. String -> Props st
charSet v _ = P.charSet v

challenge :: ∀ st. String -> Props st
challenge v _ = P.challenge v

checked :: ∀ st. Boolean -> Props st
checked v _ = P.checked v

cite :: ∀ st. String -> Props st
cite v _ = P.cite v

classID :: ∀ st. String -> Props st
classID v _ = P.classID v

className :: ∀ st. String -> Props st
className v _ = P.className v

cols :: ∀ st. Int -> Props st
cols v _ = P.cols v

colSpan :: ∀ st. Int -> Props st
colSpan v _ = P.colSpan v

content :: ∀ st. String -> Props st
content v _ = P.content v

contentEditable :: ∀ st. Boolean -> Props st
contentEditable v _ = P.contentEditable v

contextMenu :: ∀ st. String -> Props st
contextMenu v _ = P.contextMenu v

controls :: ∀ st. Boolean -> Props st
controls v _ = P.controls v

coords :: ∀ st. String -> Props st
coords v _ = P.coords v

crossOrigin :: ∀ st. String -> Props st
crossOrigin v _ = P.crossOrigin v

dateTime :: ∀ st. String -> Props st
dateTime v _ = P.dateTime v

default :: ∀ st. Boolean -> Props st
default v _ = P.default v

defaultChecked :: ∀ st. Boolean -> Props st
defaultChecked v _ = P.defaultChecked v

defaultValue :: ∀ st. String -> Props st
defaultValue v _ = P.defaultValue v

defer :: ∀ st. Boolean -> Props st
defer v _ = P.defer v

dir :: ∀ st. String -> Props st
dir v _ = P.dir v

disabled :: ∀ st. Boolean -> Props st
disabled v _ = P.disabled v

download :: ∀ st. String -> Props st
download v _ = P.download v

draggable :: ∀ st. Boolean -> Props st
draggable v _ = P.draggable v

encType :: ∀ st. String -> Props st
encType v _ = P.encType v

form :: ∀ st. String -> Props st
form v _ = P.form v

formAction :: ∀ st. String -> Props st
formAction v _ = P.formAction v

formEncType :: ∀ st. String -> Props st
formEncType v _ = P.formEncType v

formMethod :: ∀ st. String -> Props st
formMethod v _ = P.formMethod v

formNoValidate :: ∀ st. Boolean -> Props st
formNoValidate v _ = P.formNoValidate v

formTarget :: ∀ st. String -> Props st
formTarget v _ = P.formTarget v

frameBorder :: ∀ st. String -> Props st
frameBorder v _ = P.frameBorder v

headers :: ∀ st. String -> Props st
headers v _ = P.headers v

height :: ∀ st. String -> Props st
height v _ = P.height v

hidden :: ∀ st. Boolean -> Props st
hidden v _ = P.hidden v

high :: ∀ st. String -> Props st
high v _ = P.high v

href :: ∀ st. String -> Props st
href v _ = P.href v

hrefLang :: ∀ st. String -> Props st
hrefLang v _ = P.hrefLang v

htmlFor :: ∀ st. String -> Props st
htmlFor v _ = P.htmlFor v

httpEquiv :: ∀ st. String -> Props st
httpEquiv v _ = P.httpEquiv v

icon :: ∀ st. String -> Props st
icon v _ = P.icon v

_id :: ∀ st. String -> Props st
_id v _ = P._id v

inputMode :: ∀ st. String -> Props st
inputMode v _ = P.inputMode v

integrity :: ∀ st. String -> Props st
integrity v _ = P.integrity v

is :: ∀ st. String -> Props st
is v _ = P.is v

key :: ∀ st. String -> Props st
key v _ = P.key v

keyParams :: ∀ st. String -> Props st
keyParams v _ = P.keyParams v

keyType :: ∀ st. String -> Props st
keyType v _ = P.keyType v

kind :: ∀ st. String -> Props st
kind v _ = P.kind v

label :: ∀ st. String -> Props st
label v _ = P.label v

lang :: ∀ st. String -> Props st
lang v _ = P.lang v

list :: ∀ st. String -> Props st
list v _ = P.list v

loop :: ∀ st. Boolean -> Props st
loop v _ = P.loop v

low :: ∀ st. String -> Props st
low v _ = P.low v

manifest :: ∀ st. String -> Props st
manifest v _ = P.manifest v

marginHeight :: ∀ st. String -> Props st
marginHeight v _ = P.marginHeight v

marginWidth :: ∀ st. String -> Props st
marginWidth v _ = P.marginWidth v

max :: ∀ st. String -> Props st
max v _ = P.max v

maxLength :: ∀ st. String -> Props st
maxLength v _ = P.maxLength v

media :: ∀ st. String -> Props st
media v _ = P.media v

mediaGroup :: ∀ st. String -> Props st
mediaGroup v _ = P.mediaGroup v

method :: ∀ st. String -> Props st
method v _ = P.method v

min :: ∀ st. String -> Props st
min v _ = P.min v

minLength :: ∀ st. String -> Props st
minLength v _ = P.minLength v

multiple :: ∀ st. Boolean -> Props st
multiple v _ = P.multiple v

muted :: ∀ st. Boolean -> Props st
muted v _ = P.muted v

name :: ∀ st. String -> Props st
name v _ = P.name v

nonce :: ∀ st. String -> Props st
nonce v _ = P.nonce v

noValidate :: ∀ st. Boolean -> Props st
noValidate v _ = P.noValidate v

open :: ∀ st. Boolean -> Props st
open v _ = P.open v

optimum :: ∀ st. String -> Props st
optimum v _ = P.optimum v

pattern :: ∀ st. String -> Props st
pattern v _ = P.pattern v

placeholder :: ∀ st. String -> Props st
placeholder v _ = P.placeholder v

poster :: ∀ st. String -> Props st
poster v _ = P.poster v

preload :: ∀ st. String -> Props st
preload v _ = P.preload v

profile :: ∀ st. String -> Props st
profile v _ = P.profile v

radioGroup :: ∀ st. String -> Props st
radioGroup v _ = P.radioGroup v

readOnly :: ∀ st. Boolean -> Props st
readOnly v _ = P.readOnly v

rel :: ∀ st. String -> Props st
rel v _ = P.rel v

required :: ∀ st. Boolean -> Props st
required v _ = P.required v

reversed :: ∀ st. Boolean -> Props st
reversed v _ = P.reversed v

role :: ∀ st. String -> Props st
role v _ = P.role v

rows :: ∀ st. Int -> Props st
rows v _ = P.rows v

rowSpan :: ∀ st. Int -> Props st
rowSpan v _ = P.rowSpan v

sandbox :: ∀ st. String -> Props st
sandbox v _ = P.sandbox v

scope :: ∀ st. String -> Props st
scope v _ = P.scope v

scoped :: ∀ st. Boolean -> Props st
scoped v _ = P.scoped v

scrolling :: ∀ st. String -> Props st
scrolling v _ = P.scrolling v

seamless :: ∀ st. Boolean -> Props st
seamless v _ = P.seamless v

selected :: ∀ st. Boolean -> Props st
selected v _ = P.selected v

shape :: ∀ st. String -> Props st
shape v _ = P.shape v

size :: ∀ st. Int -> Props st
size v _ = P.size v

sizes :: ∀ st. String -> Props st
sizes v _ = P.sizes v

span :: ∀ st. Int -> Props st
span v _ = P.span v

spellCheck :: ∀ st. Boolean -> Props st
spellCheck v _ = P.spellCheck v

src :: ∀ st. String -> Props st
src v _ = P.src v

srcDoc :: ∀ st. String -> Props st
srcDoc v _ = P.srcDoc v

srcLang :: ∀ st. String -> Props st
srcLang v _ = P.srcLang v

srcSet :: ∀ st. String -> Props st
srcSet v _ = P.srcSet v

start :: ∀ st. Int -> Props st
start v _ = P.start v

step :: ∀ st. String -> Props st
step v _ = P.step v

summary :: ∀ st. String -> Props st
summary v _ = P.summary v

tabIndex :: ∀ st. Int -> Props st
tabIndex v _ = P.tabIndex v

target :: ∀ st. String -> Props st
target v _ = P.target v

title :: ∀ st. String -> Props st
title v _ = P.title v

_type :: ∀ st. String -> Props st
_type v _ = P._type v

useMap :: ∀ st. String -> Props st
useMap v _ = P.useMap v

value :: ∀ st. String -> Props st
value v _ = P.value v

width :: ∀ st. String -> Props st
width v _ = P.width v

wmode :: ∀ st. String -> Props st
wmode v _ = P.wmode v

wrap :: ∀ st. String -> Props st
wrap v _ = P.wrap v

-- RDFa Attributes
about :: ∀ st. String -> Props st
about v _ = P.about v

datatype :: ∀ st. String -> Props st
datatype v _ = P.datatype v

inlist :: ∀ st. String -> Props st
inlist v _ = P.inlist v

prefix :: ∀ st. String -> Props st
prefix v _ = P.prefix v

property :: ∀ st. String -> Props st
property v _ = P.property v

resource :: ∀ st. String -> Props st
resource v _ = P.resource v

typeof :: ∀ st. String -> Props st
typeof v _ = P.typeof v

vocab :: ∀ st. String -> Props st
vocab v _ = P.vocab v

-- Non-standard Attributes
autoCapitalize :: ∀ st. String -> Props st
autoCapitalize v _ = P.autoCapitalize v

autoCorrect :: ∀ st. String -> Props st
autoCorrect v _ = P.autoCorrect v

autoSave :: ∀ st. String -> Props st
autoSave v _ = P.autoSave v

color :: ∀ st. String -> Props st
color v _ = P.color v

itemProp :: ∀ st. String -> Props st
itemProp v _ = P.itemProp v

itemScope :: ∀ st. Boolean -> Props st
itemScope v _ = P.itemScope v

itemType :: ∀ st. String -> Props st
itemType v _ = P.itemType v

itemID :: ∀ st. String -> Props st
itemID v _ = P.itemID v

itemRef :: ∀ st. String -> Props st
itemRef v _ = P.itemRef v

results :: ∀ st. Int -> Props st
results v _ = P.results v

security :: ∀ st. String -> Props st
security v _ = P.security v

unselectable :: ∀ st. Boolean -> Props st
unselectable v _ = P.unselectable v

--------------------------------------------------------------------------------

onAnimationStart :: ∀ st.
  (Event.SyntheticAnimationEvent -> Effect st Unit) -> Props st
onAnimationStart f effect = P.onAnimationStart (effect ○ f)

onAnimationEnd :: ∀ st.
  (Event.SyntheticAnimationEvent -> Effect st Unit) -> Props st
onAnimationEnd f effect = P.onAnimationEnd (effect ○ f)

onAnimationIteration :: ∀ st.
  (Event.SyntheticAnimationEvent -> Effect st Unit) -> Props st
onAnimationIteration f effect = P.onAnimationIteration (effect ○ f)

onTransitionEnd :: ∀ st.
  (Event.SyntheticTransitionEvent -> Effect st Unit) -> Props st
onTransitionEnd f effect = P.onTransitionEnd (effect ○ f)

onLoad :: ∀ st.
  (Event.SyntheticEvent -> Effect st Unit) -> Props st
onLoad f effect = P.onLoad (effect ○ f)

onCopy :: ∀ st.
  (Event.SyntheticClipboardEvent -> Effect st Unit) -> Props st
onCopy f effect = P.onCopy (effect ○ f)

onCut :: ∀ st.
  (Event.SyntheticClipboardEvent -> Effect st Unit) -> Props st
onCut f effect = P.onCut (effect ○ f)

onPaste :: ∀ st.
  (Event.SyntheticClipboardEvent -> Effect st Unit) -> Props st
onPaste f effect = P.onPaste (effect ○ f)

onKeyDown :: ∀ st.
  (Event.SyntheticKeyboardEvent -> Effect st Unit) -> Props st
onKeyDown f effect = P.onKeyDown (effect ○ f)

onKeyPress :: ∀ st.
  (Event.SyntheticKeyboardEvent -> Effect st Unit) -> Props st
onKeyPress f effect = P.onKeyPress (effect ○ f)

onKeyUp :: ∀ st.
  (Event.SyntheticKeyboardEvent -> Effect st Unit) -> Props st
onKeyUp f effect = P.onKeyUp (effect ○ f)

onFocus :: ∀ st.
  (Event.SyntheticFocusEvent -> Effect st Unit) -> Props st
onFocus f effect = P.onFocus (effect ○ f)

onBlur :: ∀ st.
  (Event.SyntheticFocusEvent -> Effect st Unit) -> Props st
onBlur f effect = P.onBlur (effect ○ f)

onChange :: ∀ st.
  (Event.SyntheticInputEvent -> Effect st Unit) -> Props st
onChange f effect = P.onChange (effect ○ f)

onInput :: ∀ st.
  (Event.SyntheticInputEvent -> Effect st Unit) -> Props st
onInput f effect = P.onInput (effect ○ f)

onInvalid :: ∀ st.
  (Event.SyntheticInputEvent -> Effect st Unit) -> Props st
onInvalid f effect = P.onInvalid (effect ○ f)

onSubmit :: ∀ st.
  (Event.SyntheticInputEvent -> Effect st Unit) -> Props st
onSubmit f effect = P.onSubmit (effect ○ f)

onClick :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onClick f effect = P.onClick (effect ○ f)

onDoubleClick :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onDoubleClick f effect = P.onDoubleClick (effect ○ f)

onDrag :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onDrag f effect = P.onDrag (effect ○ f)

onDragEnd :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onDragEnd f effect = P.onDragEnd (effect ○ f)

onDragEnter :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onDragEnter f effect = P.onDragEnter (effect ○ f)

onDragExit :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onDragExit f effect = P.onDragExit (effect ○ f)

onDragLeave :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onDragLeave f effect = P.onDragLeave (effect ○ f)

onDragOver :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onDragOver f effect = P.onDragOver (effect ○ f)

onDragStart :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onDragStart f effect = P.onDragStart (effect ○ f)

onDrop :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onDrop f effect = P.onDrop (effect ○ f)

onMouseDown :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onMouseDown f effect = P.onMouseDown (effect ○ f)

onMouseEnter :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onMouseEnter f effect = P.onMouseEnter (effect ○ f)

onMouseLeave :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onMouseLeave f effect = P.onMouseLeave (effect ○ f)

onMouseMove :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onMouseMove f effect = P.onMouseMove (effect ○ f)

onMouseOut :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onMouseOut f effect = P.onMouseOut (effect ○ f)

onMouseOver :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onMouseOver f effect = P.onMouseOver (effect ○ f)

onMouseUp :: ∀ st.
  (Event.SyntheticMouseEvent -> Effect st Unit) -> Props st
onMouseUp f effect = P.onMouseUp (effect ○ f)

onTouchCancel :: ∀ st.
  (Event.SyntheticTouchEvent -> Effect st Unit) -> Props st
onTouchCancel f effect = P.onTouchCancel (effect ○ f)

onTouchEnd :: ∀ st.
  (Event.SyntheticTouchEvent -> Effect st Unit) -> Props st
onTouchEnd f effect = P.onTouchEnd (effect ○ f)

onTouchMove :: ∀ st.
  (Event.SyntheticTouchEvent -> Effect st Unit) -> Props st
onTouchMove f effect = P.onTouchMove (effect ○ f)

onTouchStart :: ∀ st.
  (Event.SyntheticTouchEvent -> Effect st Unit) -> Props st
onTouchStart f effect = P.onTouchStart (effect ○ f)

onScroll :: ∀ st.
  (Event.SyntheticUIEvent -> Effect st Unit) -> Props st
onScroll f effect = P.onScroll (effect ○ f)

onWheel :: ∀ st.
  (Event.SyntheticWheelEvent -> Effect st Unit) -> Props st
onWheel f effect = P.onWheel (effect ○ f)

--------------------------------------------------------------------------------

suppressContentEditableWarning :: ∀ st. Boolean -> Props st
suppressContentEditableWarning v _ = P.suppressContentEditableWarning v

-- SVG attributes
x :: ∀ st. Int -> Props st
x v _ = P.x v

y :: ∀ st. Int -> Props st
y v _ = P.y v

cx :: ∀ st. Int -> Props st
cx v _ = P.cx v

cy :: ∀ st. Int -> Props st
cy v _ = P.cy v

r :: ∀ st. Int -> Props st
r v _ = P.r v

fill :: ∀ st. String -> Props st
fill v _ = P.fill v

opacity :: ∀ st. Int -> Props st
opacity v _ = P.opacity v

fillOpacity :: ∀ st. Int -> Props st
fillOpacity v _ = P.fillOpacity v

stroke :: ∀ st. String -> Props st
stroke v _ = P.stroke v

strokeWidth :: ∀ st. Int -> Props st
strokeWidth v _ = P.strokeWidth v

points :: ∀ st. String -> Props st
points v _ = P.points v

d :: ∀ st. String -> Props st
d v _ = P.d v

viewBox :: ∀ st. String -> Props st
viewBox v _ = P.viewBox v

-- --------------------------------------------------------------------------------

onEnter :: ∀ st. Effect st Unit -> Props st
onEnter f effect = P.onKeyDown \e -> do
  keyCode <- Event.keyCode e
  if round keyCode == 13 then effect f else pure unit

onEscape :: ∀ st. Effect st Unit -> Props st
onEscape f effect = P.onKeyDown \e -> do
  keyCode <- Event.keyCode e
  if round keyCode == 27 then effect f else pure unit
