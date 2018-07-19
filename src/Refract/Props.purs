module Refract.Props where

import Prelude

import Data.Int (round)
import Data.Maybe (Maybe)
import React.SyntheticEvent as Event
import React.DOM.Props as P
import Refract (Effect, Props, (○))

aria :: ∀ ariaAttrs s r. { | ariaAttrs } -> Props s r
aria v _ = P.aria v

_data :: ∀ dataAttrs s r. { | dataAttrs } -> Props s r
_data v _ = P._data v

style :: ∀ style s r. { | style } -> Props s r
style v _ = P.style v

dangerouslySetInnerHTML :: ∀ s r. { __html :: String } -> Props s r
dangerouslySetInnerHTML v _ = P.dangerouslySetInnerHTML v

accept :: ∀ s r. String -> Props s r
accept v _ = P.accept v

acceptCharset :: ∀ s r. String -> Props s r
acceptCharset v _ = P.acceptCharset v

accessKey :: ∀ s r. String -> Props s r
accessKey v _ = P.accessKey v

action :: ∀ s r. String -> Props s r
action v _ = P.action v

allowFullScreen :: ∀ s r. Boolean -> Props s r
allowFullScreen v _ = P.allowFullScreen v

allowTransparency :: ∀ s r. Boolean -> Props s r
allowTransparency v _ = P.allowTransparency v

alt :: ∀ s r. String -> Props s r
alt v _ = P.alt v

async :: ∀ s r. Boolean -> Props s r
async v _ = P.async v

autoComplete :: ∀ s r. String -> Props s r
autoComplete v _ = P.autoComplete v

autoFocus :: ∀ s r. Boolean -> Props s r
autoFocus v _ = P.autoFocus v

autoPlay :: ∀ s r. Boolean -> Props s r
autoPlay v _ = P.autoPlay v

capture :: ∀ s r. Boolean -> Props s r
capture v _ = P.capture v

cellPadding :: ∀ s r. String -> Props s r
cellPadding v _ = P.cellPadding v

cellSpacing :: ∀ s r. String -> Props s r
cellSpacing v _ = P.cellSpacing v

charSet :: ∀ s r. String -> Props s r
charSet v _ = P.charSet v

challenge :: ∀ s r. String -> Props s r
challenge v _ = P.challenge v

checked :: ∀ s r. Boolean -> Props s r
checked v _ = P.checked v

cite :: ∀ s r. String -> Props s r
cite v _ = P.cite v

classID :: ∀ s r. String -> Props s r
classID v _ = P.classID v

className :: ∀ s r. String -> Props s r
className v _ = P.className v

cols :: ∀ s r. Int -> Props s r
cols v _ = P.cols v

colSpan :: ∀ s r. Int -> Props s r
colSpan v _ = P.colSpan v

content :: ∀ s r. String -> Props s r
content v _ = P.content v

contentEditable :: ∀ s r. Boolean -> Props s r
contentEditable v _ = P.contentEditable v

contextMenu :: ∀ s r. String -> Props s r
contextMenu v _ = P.contextMenu v

controls :: ∀ s r. Boolean -> Props s r
controls v _ = P.controls v

coords :: ∀ s r. String -> Props s r
coords v _ = P.coords v

crossOrigin :: ∀ s r. String -> Props s r
crossOrigin v _ = P.crossOrigin v

dateTime :: ∀ s r. String -> Props s r
dateTime v _ = P.dateTime v

default :: ∀ s r. Boolean -> Props s r
default v _ = P.default v

defaultChecked :: ∀ s r. Boolean -> Props s r
defaultChecked v _ = P.defaultChecked v

defaultValue :: ∀ s r. String -> Props s r
defaultValue v _ = P.defaultValue v

defer :: ∀ s r. Boolean -> Props s r
defer v _ = P.defer v

dir :: ∀ s r. String -> Props s r
dir v _ = P.dir v

disabled :: ∀ s r. Boolean -> Props s r
disabled v _ = P.disabled v

download :: ∀ s r. String -> Props s r
download v _ = P.download v

draggable :: ∀ s r. Boolean -> Props s r
draggable v _ = P.draggable v

encType :: ∀ s r. String -> Props s r
encType v _ = P.encType v

form :: ∀ s r. String -> Props s r
form v _ = P.form v

formAction :: ∀ s r. String -> Props s r
formAction v _ = P.formAction v

formEncType :: ∀ s r. String -> Props s r
formEncType v _ = P.formEncType v

formMethod :: ∀ s r. String -> Props s r
formMethod v _ = P.formMethod v

formNoValidate :: ∀ s r. Boolean -> Props s r
formNoValidate v _ = P.formNoValidate v

formTarget :: ∀ s r. String -> Props s r
formTarget v _ = P.formTarget v

frameBorder :: ∀ s r. String -> Props s r
frameBorder v _ = P.frameBorder v

headers :: ∀ s r. String -> Props s r
headers v _ = P.headers v

height :: ∀ s r. String -> Props s r
height v _ = P.height v

hidden :: ∀ s r. Boolean -> Props s r
hidden v _ = P.hidden v

high :: ∀ s r. String -> Props s r
high v _ = P.high v

href :: ∀ s r. String -> Props s r
href v _ = P.href v

hrefLang :: ∀ s r. String -> Props s r
hrefLang v _ = P.hrefLang v

htmlFor :: ∀ s r. String -> Props s r
htmlFor v _ = P.htmlFor v

httpEquiv :: ∀ s r. String -> Props s r
httpEquiv v _ = P.httpEquiv v

icon :: ∀ s r. String -> Props s r
icon v _ = P.icon v

_id :: ∀ s r. String -> Props s r
_id v _ = P._id v

inputMode :: ∀ s r. String -> Props s r
inputMode v _ = P.inputMode v

integrity :: ∀ s r. String -> Props s r
integrity v _ = P.integrity v

is :: ∀ s r. String -> Props s r
is v _ = P.is v

key :: ∀ s r. String -> Props s r
key v _ = P.key v

keyParams :: ∀ s r. String -> Props s r
keyParams v _ = P.keyParams v

keyType :: ∀ s r. String -> Props s r
keyType v _ = P.keyType v

kind :: ∀ s r. String -> Props s r
kind v _ = P.kind v

label :: ∀ s r. String -> Props s r
label v _ = P.label v

lang :: ∀ s r. String -> Props s r
lang v _ = P.lang v

list :: ∀ s r. String -> Props s r
list v _ = P.list v

loop :: ∀ s r. Boolean -> Props s r
loop v _ = P.loop v

low :: ∀ s r. String -> Props s r
low v _ = P.low v

manifest :: ∀ s r. String -> Props s r
manifest v _ = P.manifest v

marginHeight :: ∀ s r. String -> Props s r
marginHeight v _ = P.marginHeight v

marginWidth :: ∀ s r. String -> Props s r
marginWidth v _ = P.marginWidth v

max :: ∀ s r. String -> Props s r
max v _ = P.max v

maxLength :: ∀ s r. String -> Props s r
maxLength v _ = P.maxLength v

media :: ∀ s r. String -> Props s r
media v _ = P.media v

mediaGroup :: ∀ s r. String -> Props s r
mediaGroup v _ = P.mediaGroup v

method :: ∀ s r. String -> Props s r
method v _ = P.method v

min :: ∀ s r. String -> Props s r
min v _ = P.min v

minLength :: ∀ s r. String -> Props s r
minLength v _ = P.minLength v

multiple :: ∀ s r. Boolean -> Props s r
multiple v _ = P.multiple v

muted :: ∀ s r. Boolean -> Props s r
muted v _ = P.muted v

name :: ∀ s r. String -> Props s r
name v _ = P.name v

nonce :: ∀ s r. String -> Props s r
nonce v _ = P.nonce v

noValidate :: ∀ s r. Boolean -> Props s r
noValidate v _ = P.noValidate v

open :: ∀ s r. Boolean -> Props s r
open v _ = P.open v

optimum :: ∀ s r. String -> Props s r
optimum v _ = P.optimum v

pattern :: ∀ s r. String -> Props s r
pattern v _ = P.pattern v

placeholder :: ∀ s r. String -> Props s r
placeholder v _ = P.placeholder v

poster :: ∀ s r. String -> Props s r
poster v _ = P.poster v

preload :: ∀ s r. String -> Props s r
preload v _ = P.preload v

profile :: ∀ s r. String -> Props s r
profile v _ = P.profile v

radioGroup :: ∀ s r. String -> Props s r
radioGroup v _ = P.radioGroup v

readOnly :: ∀ s r. Boolean -> Props s r
readOnly v _ = P.readOnly v

rel :: ∀ s r. String -> Props s r
rel v _ = P.rel v

required :: ∀ s r. Boolean -> Props s r
required v _ = P.required v

reversed :: ∀ s r. Boolean -> Props s r
reversed v _ = P.reversed v

role :: ∀ s r. String -> Props s r
role v _ = P.role v

rows :: ∀ s r. Int -> Props s r
rows v _ = P.rows v

rowSpan :: ∀ s r. Int -> Props s r
rowSpan v _ = P.rowSpan v

sandbox :: ∀ s r. String -> Props s r
sandbox v _ = P.sandbox v

scope :: ∀ s r. String -> Props s r
scope v _ = P.scope v

scoped :: ∀ s r. Boolean -> Props s r
scoped v _ = P.scoped v

scrolling :: ∀ s r. String -> Props s r
scrolling v _ = P.scrolling v

seamless :: ∀ s r. Boolean -> Props s r
seamless v _ = P.seamless v

selected :: ∀ s r. Boolean -> Props s r
selected v _ = P.selected v

shape :: ∀ s r. String -> Props s r
shape v _ = P.shape v

size :: ∀ s r. Int -> Props s r
size v _ = P.size v

sizes :: ∀ s r. String -> Props s r
sizes v _ = P.sizes v

span :: ∀ s r. Int -> Props s r
span v _ = P.span v

spellCheck :: ∀ s r. Boolean -> Props s r
spellCheck v _ = P.spellCheck v

src :: ∀ s r. String -> Props s r
src v _ = P.src v

srcDoc :: ∀ s r. String -> Props s r
srcDoc v _ = P.srcDoc v

srcLang :: ∀ s r. String -> Props s r
srcLang v _ = P.srcLang v

srcSet :: ∀ s r. String -> Props s r
srcSet v _ = P.srcSet v

start :: ∀ s r. Int -> Props s r
start v _ = P.start v

step :: ∀ s r. String -> Props s r
step v _ = P.step v

summary :: ∀ s r. String -> Props s r
summary v _ = P.summary v

tabIndex :: ∀ s r. Int -> Props s r
tabIndex v _ = P.tabIndex v

target :: ∀ s r. String -> Props s r
target v _ = P.target v

title :: ∀ s r. String -> Props s r
title v _ = P.title v

_type :: ∀ s r. String -> Props s r
_type v _ = P._type v

useMap :: ∀ s r. String -> Props s r
useMap v _ = P.useMap v

value :: ∀ s r. String -> Props s r
value v _ = P.value v

width :: ∀ s r. String -> Props s r
width v _ = P.width v

wmode :: ∀ s r. String -> Props s r
wmode v _ = P.wmode v

wrap :: ∀ s r. String -> Props s r
wrap v _ = P.wrap v

-- RDFa Attributes
about :: ∀ s r. String -> Props s r
about v _ = P.about v

datatype :: ∀ s r. String -> Props s r
datatype v _ = P.datatype v

inlist :: ∀ s r. String -> Props s r
inlist v _ = P.inlist v

prefix :: ∀ s r. String -> Props s r
prefix v _ = P.prefix v

property :: ∀ s r. String -> Props s r
property v _ = P.property v

resource :: ∀ s r. String -> Props s r
resource v _ = P.resource v

typeof :: ∀ s r. String -> Props s r
typeof v _ = P.typeof v

vocab :: ∀ s r. String -> Props s r
vocab v _ = P.vocab v

-- Non-standard Attributes
autoCapitalize :: ∀ s r. String -> Props s r
autoCapitalize v _ = P.autoCapitalize v

autoCorrect :: ∀ s r. String -> Props s r
autoCorrect v _ = P.autoCorrect v

autoSave :: ∀ s r. String -> Props s r
autoSave v _ = P.autoSave v

color :: ∀ s r. String -> Props s r
color v _ = P.color v

itemProp :: ∀ s r. String -> Props s r
itemProp v _ = P.itemProp v

itemScope :: ∀ s r. Boolean -> Props s r
itemScope v _ = P.itemScope v

itemType :: ∀ s r. String -> Props s r
itemType v _ = P.itemType v

itemID :: ∀ s r. String -> Props s r
itemID v _ = P.itemID v

itemRef :: ∀ s r. String -> Props s r
itemRef v _ = P.itemRef v

results :: ∀ s r. Int -> Props s r
results v _ = P.results v

security :: ∀ s r. String -> Props s r
security v _ = P.security v

unselectable :: ∀ s r. Boolean -> Props s r
unselectable v _ = P.unselectable v

--------------------------------------------------------------------------------

onAnimationStart :: ∀ s r.
  (Event.SyntheticAnimationEvent -> Effect s (Maybe r)) -> Props s r
onAnimationStart f effect = P.onAnimationStart (effect ○ f)

onAnimationEnd :: ∀ s r.
  (Event.SyntheticAnimationEvent -> Effect s (Maybe r)) -> Props s r
onAnimationEnd f effect = P.onAnimationEnd (effect ○ f)

onAnimationIteration :: ∀ s r.
  (Event.SyntheticAnimationEvent -> Effect s (Maybe r)) -> Props s r
onAnimationIteration f effect = P.onAnimationIteration (effect ○ f)

onTransitionEnd :: ∀ s r.
  (Event.SyntheticTransitionEvent -> Effect s (Maybe r)) -> Props s r
onTransitionEnd f effect = P.onTransitionEnd (effect ○ f)

onLoad :: ∀ s r.
  (Event.SyntheticEvent -> Effect s (Maybe r)) -> Props s r
onLoad f effect = P.onLoad (effect ○ f)

onCopy :: ∀ s r.
  (Event.SyntheticClipboardEvent -> Effect s (Maybe r)) -> Props s r
onCopy f effect = P.onCopy (effect ○ f)

onCut :: ∀ s r.
  (Event.SyntheticClipboardEvent -> Effect s (Maybe r)) -> Props s r
onCut f effect = P.onCut (effect ○ f)

onPaste :: ∀ s r.
  (Event.SyntheticClipboardEvent -> Effect s (Maybe r)) -> Props s r
onPaste f effect = P.onPaste (effect ○ f)

onKeyDown :: ∀ s r.
  (Event.SyntheticKeyboardEvent -> Effect s (Maybe r)) -> Props s r
onKeyDown f effect = P.onKeyDown (effect ○ f)

onKeyPress :: ∀ s r.
  (Event.SyntheticKeyboardEvent -> Effect s (Maybe r)) -> Props s r
onKeyPress f effect = P.onKeyPress (effect ○ f)

onKeyUp :: ∀ s r.
  (Event.SyntheticKeyboardEvent -> Effect s (Maybe r)) -> Props s r
onKeyUp f effect = P.onKeyUp (effect ○ f)

onFocus :: ∀ s r.
  (Event.SyntheticFocusEvent -> Effect s (Maybe r)) -> Props s r
onFocus f effect = P.onFocus (effect ○ f)

onBlur :: ∀ s r.
  (Event.SyntheticFocusEvent -> Effect s (Maybe r)) -> Props s r
onBlur f effect = P.onBlur (effect ○ f)

onChange :: ∀ s r.
  (Event.SyntheticInputEvent -> Effect s (Maybe r)) -> Props s r
onChange f effect = P.onChange (effect ○ f)

onInput :: ∀ s r.
  (Event.SyntheticInputEvent -> Effect s (Maybe r)) -> Props s r
onInput f effect = P.onInput (effect ○ f)

onInvalid :: ∀ s r.
  (Event.SyntheticInputEvent -> Effect s (Maybe r)) -> Props s r
onInvalid f effect = P.onInvalid (effect ○ f)

onSubmit :: ∀ s r.
  (Event.SyntheticInputEvent -> Effect s (Maybe r)) -> Props s r
onSubmit f effect = P.onSubmit (effect ○ f)

onClick :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onClick f effect = P.onClick (effect ○ f)

onDoubleClick :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onDoubleClick f effect = P.onDoubleClick (effect ○ f)

onDrag :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onDrag f effect = P.onDrag (effect ○ f)

onDragEnd :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onDragEnd f effect = P.onDragEnd (effect ○ f)

onDragEnter :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onDragEnter f effect = P.onDragEnter (effect ○ f)

onDragExit :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onDragExit f effect = P.onDragExit (effect ○ f)

onDragLeave :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onDragLeave f effect = P.onDragLeave (effect ○ f)

onDragOver :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onDragOver f effect = P.onDragOver (effect ○ f)

onDragStart :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onDragStart f effect = P.onDragStart (effect ○ f)

onDrop :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onDrop f effect = P.onDrop (effect ○ f)

onMouseDown :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onMouseDown f effect = P.onMouseDown (effect ○ f)

onMouseEnter :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onMouseEnter f effect = P.onMouseEnter (effect ○ f)

onMouseLeave :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onMouseLeave f effect = P.onMouseLeave (effect ○ f)

onMouseMove :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onMouseMove f effect = P.onMouseMove (effect ○ f)

onMouseOut :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onMouseOut f effect = P.onMouseOut (effect ○ f)

onMouseOver :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onMouseOver f effect = P.onMouseOver (effect ○ f)

onMouseUp :: ∀ s r.
  (Event.SyntheticMouseEvent -> Effect s (Maybe r)) -> Props s r
onMouseUp f effect = P.onMouseUp (effect ○ f)

onTouchCancel :: ∀ s r.
  (Event.SyntheticTouchEvent -> Effect s (Maybe r)) -> Props s r
onTouchCancel f effect = P.onTouchCancel (effect ○ f)

onTouchEnd :: ∀ s r.
  (Event.SyntheticTouchEvent -> Effect s (Maybe r)) -> Props s r
onTouchEnd f effect = P.onTouchEnd (effect ○ f)

onTouchMove :: ∀ s r.
  (Event.SyntheticTouchEvent -> Effect s (Maybe r)) -> Props s r
onTouchMove f effect = P.onTouchMove (effect ○ f)

onTouchStart :: ∀ s r.
  (Event.SyntheticTouchEvent -> Effect s (Maybe r)) -> Props s r
onTouchStart f effect = P.onTouchStart (effect ○ f)

onScroll :: ∀ s r.
  (Event.SyntheticUIEvent -> Effect s (Maybe r)) -> Props s r
onScroll f effect = P.onScroll (effect ○ f)

onWheel :: ∀ s r.
  (Event.SyntheticWheelEvent -> Effect s (Maybe r)) -> Props s r
onWheel f effect = P.onWheel (effect ○ f)

--------------------------------------------------------------------------------

suppressContentEditableWarning :: ∀ s r. Boolean -> Props s r
suppressContentEditableWarning v _ = P.suppressContentEditableWarning v

-- SVG attributes
x :: ∀ s r. Int -> Props s r
x v _ = P.x v

y :: ∀ s r. Int -> Props s r
y v _ = P.y v

cx :: ∀ s r. Int -> Props s r
cx v _ = P.cx v

cy :: ∀ s r. Int -> Props s r
cy v _ = P.cy v

r :: ∀ s r. Int -> Props s r
r v _ = P.r v

fill :: ∀ s r. String -> Props s r
fill v _ = P.fill v

opacity :: ∀ s r. Int -> Props s r
opacity v _ = P.opacity v

fillOpacity :: ∀ s r. Int -> Props s r
fillOpacity v _ = P.fillOpacity v

stroke :: ∀ s r. String -> Props s r
stroke v _ = P.stroke v

strokeWidth :: ∀ s r. Int -> Props s r
strokeWidth v _ = P.strokeWidth v

points :: ∀ s r. String -> Props s r
points v _ = P.points v

d :: ∀ s r. String -> Props s r
d v _ = P.d v

viewBox :: ∀ s r. String -> Props s r
viewBox v _ = P.viewBox v

-- --------------------------------------------------------------------------------

onEnter :: ∀ s r. Effect s (Maybe r) -> Props s r
onEnter f effect = P.onKeyDown \e -> do
  keyCode <- Event.keyCode e
  if round keyCode == 13 then effect f else pure unit

onEscape :: ∀ s r. Effect s (Maybe r) -> Props s r
onEscape f effect = P.onKeyDown \e -> do
  keyCode <- Event.keyCode e
  if round keyCode == 27 then effect f else pure unit
