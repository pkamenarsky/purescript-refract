module Refract.Props where

import Prelude

import Data.Int (round)
import React.SyntheticEvent as Event
import React.DOM.Props as P
import Refract (Effect, Props, (○))

aria :: ∀ ariaAttrs s. { | ariaAttrs } -> Props s
aria v _ = P.aria v

_data :: ∀ dataAttrs s. { | dataAttrs } -> Props s
_data v _ = P._data v

style :: ∀ style s. { | style } -> Props s
style v _ = P.style v

dangerouslySetInnerHTML :: ∀ s. { __html :: String } -> Props s
dangerouslySetInnerHTML v _ = P.dangerouslySetInnerHTML v

accept :: ∀ s. String -> Props s
accept v _ = P.accept v

acceptCharset :: ∀ s. String -> Props s
acceptCharset v _ = P.acceptCharset v

accessKey :: ∀ s. String -> Props s
accessKey v _ = P.accessKey v

action :: ∀ s. String -> Props s
action v _ = P.action v

allowFullScreen :: ∀ s. Boolean -> Props s
allowFullScreen v _ = P.allowFullScreen v

allowTransparency :: ∀ s. Boolean -> Props s
allowTransparency v _ = P.allowTransparency v

alt :: ∀ s. String -> Props s
alt v _ = P.alt v

async :: ∀ s. Boolean -> Props s
async v _ = P.async v

autoComplete :: ∀ s. String -> Props s
autoComplete v _ = P.autoComplete v

autoFocus :: ∀ s. Boolean -> Props s
autoFocus v _ = P.autoFocus v

autoPlay :: ∀ s. Boolean -> Props s
autoPlay v _ = P.autoPlay v

capture :: ∀ s. Boolean -> Props s
capture v _ = P.capture v

cellPadding :: ∀ s. String -> Props s
cellPadding v _ = P.cellPadding v

cellSpacing :: ∀ s. String -> Props s
cellSpacing v _ = P.cellSpacing v

charSet :: ∀ s. String -> Props s
charSet v _ = P.charSet v

challenge :: ∀ s. String -> Props s
challenge v _ = P.challenge v

checked :: ∀ s. Boolean -> Props s
checked v _ = P.checked v

cite :: ∀ s. String -> Props s
cite v _ = P.cite v

classID :: ∀ s. String -> Props s
classID v _ = P.classID v

className :: ∀ s. String -> Props s
className v _ = P.className v

cols :: ∀ s. Int -> Props s
cols v _ = P.cols v

colSpan :: ∀ s. Int -> Props s
colSpan v _ = P.colSpan v

content :: ∀ s. String -> Props s
content v _ = P.content v

contentEditable :: ∀ s. Boolean -> Props s
contentEditable v _ = P.contentEditable v

contextMenu :: ∀ s. String -> Props s
contextMenu v _ = P.contextMenu v

controls :: ∀ s. Boolean -> Props s
controls v _ = P.controls v

coords :: ∀ s. String -> Props s
coords v _ = P.coords v

crossOrigin :: ∀ s. String -> Props s
crossOrigin v _ = P.crossOrigin v

dateTime :: ∀ s. String -> Props s
dateTime v _ = P.dateTime v

default :: ∀ s. Boolean -> Props s
default v _ = P.default v

defaultChecked :: ∀ s. Boolean -> Props s
defaultChecked v _ = P.defaultChecked v

defaultValue :: ∀ s. String -> Props s
defaultValue v _ = P.defaultValue v

defer :: ∀ s. Boolean -> Props s
defer v _ = P.defer v

dir :: ∀ s. String -> Props s
dir v _ = P.dir v

disabled :: ∀ s. Boolean -> Props s
disabled v _ = P.disabled v

download :: ∀ s. String -> Props s
download v _ = P.download v

draggable :: ∀ s. Boolean -> Props s
draggable v _ = P.draggable v

encType :: ∀ s. String -> Props s
encType v _ = P.encType v

form :: ∀ s. String -> Props s
form v _ = P.form v

formAction :: ∀ s. String -> Props s
formAction v _ = P.formAction v

formEncType :: ∀ s. String -> Props s
formEncType v _ = P.formEncType v

formMethod :: ∀ s. String -> Props s
formMethod v _ = P.formMethod v

formNoValidate :: ∀ s. Boolean -> Props s
formNoValidate v _ = P.formNoValidate v

formTarget :: ∀ s. String -> Props s
formTarget v _ = P.formTarget v

frameBorder :: ∀ s. String -> Props s
frameBorder v _ = P.frameBorder v

headers :: ∀ s. String -> Props s
headers v _ = P.headers v

height :: ∀ s. String -> Props s
height v _ = P.height v

hidden :: ∀ s. Boolean -> Props s
hidden v _ = P.hidden v

high :: ∀ s. String -> Props s
high v _ = P.high v

href :: ∀ s. String -> Props s
href v _ = P.href v

hrefLang :: ∀ s. String -> Props s
hrefLang v _ = P.hrefLang v

htmlFor :: ∀ s. String -> Props s
htmlFor v _ = P.htmlFor v

httpEquiv :: ∀ s. String -> Props s
httpEquiv v _ = P.httpEquiv v

icon :: ∀ s. String -> Props s
icon v _ = P.icon v

_id :: ∀ s. String -> Props s
_id v _ = P._id v

inputMode :: ∀ s. String -> Props s
inputMode v _ = P.inputMode v

integrity :: ∀ s. String -> Props s
integrity v _ = P.integrity v

is :: ∀ s. String -> Props s
is v _ = P.is v

key :: ∀ s. String -> Props s
key v _ = P.key v

keyParams :: ∀ s. String -> Props s
keyParams v _ = P.keyParams v

keyType :: ∀ s. String -> Props s
keyType v _ = P.keyType v

kind :: ∀ s. String -> Props s
kind v _ = P.kind v

label :: ∀ s. String -> Props s
label v _ = P.label v

lang :: ∀ s. String -> Props s
lang v _ = P.lang v

list :: ∀ s. String -> Props s
list v _ = P.list v

loop :: ∀ s. Boolean -> Props s
loop v _ = P.loop v

low :: ∀ s. String -> Props s
low v _ = P.low v

manifest :: ∀ s. String -> Props s
manifest v _ = P.manifest v

marginHeight :: ∀ s. String -> Props s
marginHeight v _ = P.marginHeight v

marginWidth :: ∀ s. String -> Props s
marginWidth v _ = P.marginWidth v

max :: ∀ s. String -> Props s
max v _ = P.max v

maxLength :: ∀ s. String -> Props s
maxLength v _ = P.maxLength v

media :: ∀ s. String -> Props s
media v _ = P.media v

mediaGroup :: ∀ s. String -> Props s
mediaGroup v _ = P.mediaGroup v

method :: ∀ s. String -> Props s
method v _ = P.method v

min :: ∀ s. String -> Props s
min v _ = P.min v

minLength :: ∀ s. String -> Props s
minLength v _ = P.minLength v

multiple :: ∀ s. Boolean -> Props s
multiple v _ = P.multiple v

muted :: ∀ s. Boolean -> Props s
muted v _ = P.muted v

name :: ∀ s. String -> Props s
name v _ = P.name v

nonce :: ∀ s. String -> Props s
nonce v _ = P.nonce v

noValidate :: ∀ s. Boolean -> Props s
noValidate v _ = P.noValidate v

open :: ∀ s. Boolean -> Props s
open v _ = P.open v

optimum :: ∀ s. String -> Props s
optimum v _ = P.optimum v

pattern :: ∀ s. String -> Props s
pattern v _ = P.pattern v

placeholder :: ∀ s. String -> Props s
placeholder v _ = P.placeholder v

poster :: ∀ s. String -> Props s
poster v _ = P.poster v

preload :: ∀ s. String -> Props s
preload v _ = P.preload v

profile :: ∀ s. String -> Props s
profile v _ = P.profile v

radioGroup :: ∀ s. String -> Props s
radioGroup v _ = P.radioGroup v

readOnly :: ∀ s. Boolean -> Props s
readOnly v _ = P.readOnly v

rel :: ∀ s. String -> Props s
rel v _ = P.rel v

required :: ∀ s. Boolean -> Props s
required v _ = P.required v

reversed :: ∀ s. Boolean -> Props s
reversed v _ = P.reversed v

role :: ∀ s. String -> Props s
role v _ = P.role v

rows :: ∀ s. Int -> Props s
rows v _ = P.rows v

rowSpan :: ∀ s. Int -> Props s
rowSpan v _ = P.rowSpan v

sandbox :: ∀ s. String -> Props s
sandbox v _ = P.sandbox v

scope :: ∀ s. String -> Props s
scope v _ = P.scope v

scoped :: ∀ s. Boolean -> Props s
scoped v _ = P.scoped v

scrolling :: ∀ s. String -> Props s
scrolling v _ = P.scrolling v

seamless :: ∀ s. Boolean -> Props s
seamless v _ = P.seamless v

selected :: ∀ s. Boolean -> Props s
selected v _ = P.selected v

shape :: ∀ s. String -> Props s
shape v _ = P.shape v

size :: ∀ s. Int -> Props s
size v _ = P.size v

sizes :: ∀ s. String -> Props s
sizes v _ = P.sizes v

span :: ∀ s. Int -> Props s
span v _ = P.span v

spellCheck :: ∀ s. Boolean -> Props s
spellCheck v _ = P.spellCheck v

src :: ∀ s. String -> Props s
src v _ = P.src v

srcDoc :: ∀ s. String -> Props s
srcDoc v _ = P.srcDoc v

srcLang :: ∀ s. String -> Props s
srcLang v _ = P.srcLang v

srcSet :: ∀ s. String -> Props s
srcSet v _ = P.srcSet v

start :: ∀ s. Int -> Props s
start v _ = P.start v

step :: ∀ s. String -> Props s
step v _ = P.step v

summary :: ∀ s. String -> Props s
summary v _ = P.summary v

tabIndex :: ∀ s. Int -> Props s
tabIndex v _ = P.tabIndex v

target :: ∀ s. String -> Props s
target v _ = P.target v

title :: ∀ s. String -> Props s
title v _ = P.title v

_type :: ∀ s. String -> Props s
_type v _ = P._type v

useMap :: ∀ s. String -> Props s
useMap v _ = P.useMap v

value :: ∀ s. String -> Props s
value v _ = P.value v

width :: ∀ s. String -> Props s
width v _ = P.width v

wmode :: ∀ s. String -> Props s
wmode v _ = P.wmode v

wrap :: ∀ s. String -> Props s
wrap v _ = P.wrap v

-- RDFa Attributes
about :: ∀ s. String -> Props s
about v _ = P.about v

datatype :: ∀ s. String -> Props s
datatype v _ = P.datatype v

inlist :: ∀ s. String -> Props s
inlist v _ = P.inlist v

prefix :: ∀ s. String -> Props s
prefix v _ = P.prefix v

property :: ∀ s. String -> Props s
property v _ = P.property v

resource :: ∀ s. String -> Props s
resource v _ = P.resource v

typeof :: ∀ s. String -> Props s
typeof v _ = P.typeof v

vocab :: ∀ s. String -> Props s
vocab v _ = P.vocab v

-- Non-standard Attributes
autoCapitalize :: ∀ s. String -> Props s
autoCapitalize v _ = P.autoCapitalize v

autoCorrect :: ∀ s. String -> Props s
autoCorrect v _ = P.autoCorrect v

autoSave :: ∀ s. String -> Props s
autoSave v _ = P.autoSave v

color :: ∀ s. String -> Props s
color v _ = P.color v

itemProp :: ∀ s. String -> Props s
itemProp v _ = P.itemProp v

itemScope :: ∀ s. Boolean -> Props s
itemScope v _ = P.itemScope v

itemType :: ∀ s. String -> Props s
itemType v _ = P.itemType v

itemID :: ∀ s. String -> Props s
itemID v _ = P.itemID v

itemRef :: ∀ s. String -> Props s
itemRef v _ = P.itemRef v

results :: ∀ s. Int -> Props s
results v _ = P.results v

security :: ∀ s. String -> Props s
security v _ = P.security v

unselectable :: ∀ s. Boolean -> Props s
unselectable v _ = P.unselectable v

--------------------------------------------------------------------------------

onAnimationStart :: ∀ s.
  (Event.SyntheticAnimationEvent -> Effect s Unit) -> Props s
onAnimationStart f effect = P.onAnimationStart (effect ○ f)

onAnimationEnd :: ∀ s.
  (Event.SyntheticAnimationEvent -> Effect s Unit) -> Props s
onAnimationEnd f effect = P.onAnimationEnd (effect ○ f)

onAnimationIteration :: ∀ s.
  (Event.SyntheticAnimationEvent -> Effect s Unit) -> Props s
onAnimationIteration f effect = P.onAnimationIteration (effect ○ f)

onTransitionEnd :: ∀ s.
  (Event.SyntheticTransitionEvent -> Effect s Unit) -> Props s
onTransitionEnd f effect = P.onTransitionEnd (effect ○ f)

onLoad :: ∀ s.
  (Event.SyntheticEvent -> Effect s Unit) -> Props s
onLoad f effect = P.onLoad (effect ○ f)

onCopy :: ∀ s.
  (Event.SyntheticClipboardEvent -> Effect s Unit) -> Props s
onCopy f effect = P.onCopy (effect ○ f)

onCut :: ∀ s.
  (Event.SyntheticClipboardEvent -> Effect s Unit) -> Props s
onCut f effect = P.onCut (effect ○ f)

onPaste :: ∀ s.
  (Event.SyntheticClipboardEvent -> Effect s Unit) -> Props s
onPaste f effect = P.onPaste (effect ○ f)

onKeyDown :: ∀ s.
  (Event.SyntheticKeyboardEvent -> Effect s Unit) -> Props s
onKeyDown f effect = P.onKeyDown (effect ○ f)

onKeyPress :: ∀ s.
  (Event.SyntheticKeyboardEvent -> Effect s Unit) -> Props s
onKeyPress f effect = P.onKeyPress (effect ○ f)

onKeyUp :: ∀ s.
  (Event.SyntheticKeyboardEvent -> Effect s Unit) -> Props s
onKeyUp f effect = P.onKeyUp (effect ○ f)

onFocus :: ∀ s.
  (Event.SyntheticFocusEvent -> Effect s Unit) -> Props s
onFocus f effect = P.onFocus (effect ○ f)

onBlur :: ∀ s.
  (Event.SyntheticFocusEvent -> Effect s Unit) -> Props s
onBlur f effect = P.onBlur (effect ○ f)

onChange :: ∀ s.
  (Event.SyntheticInputEvent -> Effect s Unit) -> Props s
onChange f effect = P.onChange (effect ○ f)

onInput :: ∀ s.
  (Event.SyntheticInputEvent -> Effect s Unit) -> Props s
onInput f effect = P.onInput (effect ○ f)

onInvalid :: ∀ s.
  (Event.SyntheticInputEvent -> Effect s Unit) -> Props s
onInvalid f effect = P.onInvalid (effect ○ f)

onSubmit :: ∀ s.
  (Event.SyntheticInputEvent -> Effect s Unit) -> Props s
onSubmit f effect = P.onSubmit (effect ○ f)

onClick :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onClick f effect = P.onClick (effect ○ f)

onDoubleClick :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onDoubleClick f effect = P.onDoubleClick (effect ○ f)

onDrag :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onDrag f effect = P.onDrag (effect ○ f)

onDragEnd :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onDragEnd f effect = P.onDragEnd (effect ○ f)

onDragEnter :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onDragEnter f effect = P.onDragEnter (effect ○ f)

onDragExit :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onDragExit f effect = P.onDragExit (effect ○ f)

onDragLeave :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onDragLeave f effect = P.onDragLeave (effect ○ f)

onDragOver :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onDragOver f effect = P.onDragOver (effect ○ f)

onDragStart :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onDragStart f effect = P.onDragStart (effect ○ f)

onDrop :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onDrop f effect = P.onDrop (effect ○ f)

onMouseDown :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onMouseDown f effect = P.onMouseDown (effect ○ f)

onMouseEnter :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onMouseEnter f effect = P.onMouseEnter (effect ○ f)

onMouseLeave :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onMouseLeave f effect = P.onMouseLeave (effect ○ f)

onMouseMove :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onMouseMove f effect = P.onMouseMove (effect ○ f)

onMouseOut :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onMouseOut f effect = P.onMouseOut (effect ○ f)

onMouseOver :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onMouseOver f effect = P.onMouseOver (effect ○ f)

onMouseUp :: ∀ s.
  (Event.SyntheticMouseEvent -> Effect s Unit) -> Props s
onMouseUp f effect = P.onMouseUp (effect ○ f)

onTouchCancel :: ∀ s.
  (Event.SyntheticTouchEvent -> Effect s Unit) -> Props s
onTouchCancel f effect = P.onTouchCancel (effect ○ f)

onTouchEnd :: ∀ s.
  (Event.SyntheticTouchEvent -> Effect s Unit) -> Props s
onTouchEnd f effect = P.onTouchEnd (effect ○ f)

onTouchMove :: ∀ s.
  (Event.SyntheticTouchEvent -> Effect s Unit) -> Props s
onTouchMove f effect = P.onTouchMove (effect ○ f)

onTouchStart :: ∀ s.
  (Event.SyntheticTouchEvent -> Effect s Unit) -> Props s
onTouchStart f effect = P.onTouchStart (effect ○ f)

onScroll :: ∀ s.
  (Event.SyntheticUIEvent -> Effect s Unit) -> Props s
onScroll f effect = P.onScroll (effect ○ f)

onWheel :: ∀ s.
  (Event.SyntheticWheelEvent -> Effect s Unit) -> Props s
onWheel f effect = P.onWheel (effect ○ f)

--------------------------------------------------------------------------------

suppressContentEditableWarning :: ∀ s. Boolean -> Props s
suppressContentEditableWarning v _ = P.suppressContentEditableWarning v

-- SVG attributes
x :: ∀ s. Int -> Props s
x v _ = P.x v

y :: ∀ s. Int -> Props s
y v _ = P.y v

cx :: ∀ s. Int -> Props s
cx v _ = P.cx v

cy :: ∀ s. Int -> Props s
cy v _ = P.cy v

r :: ∀ s. Int -> Props s
r v _ = P.r v

fill :: ∀ s. String -> Props s
fill v _ = P.fill v

opacity :: ∀ s. Int -> Props s
opacity v _ = P.opacity v

fillOpacity :: ∀ s. Int -> Props s
fillOpacity v _ = P.fillOpacity v

stroke :: ∀ s. String -> Props s
stroke v _ = P.stroke v

strokeWidth :: ∀ s. Int -> Props s
strokeWidth v _ = P.strokeWidth v

points :: ∀ s. String -> Props s
points v _ = P.points v

d :: ∀ s. String -> Props s
d v _ = P.d v

viewBox :: ∀ s. String -> Props s
viewBox v _ = P.viewBox v

-- --------------------------------------------------------------------------------

onEnter :: ∀ s. Effect s Unit -> Props s
onEnter f effect = P.onKeyDown \e -> do
  keyCode <- Event.keyCode e
  if round keyCode == 13 then effect f else pure unit

onEscape :: ∀ s. Effect s Unit -> Props s
onEscape f effect = P.onKeyDown \e -> do
  keyCode <- Event.keyCode e
  if round keyCode == 27 then effect f else pure unit
