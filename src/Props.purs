module Props where

import Prelude

import React (Event, KeyboardEvent, MouseEvent)
import React.DOM.Props as P
import Refract (Effect, Props, (○))
import Unsafe.Coerce (unsafeCoerce)

aria :: ∀ ariaAttrs eff st. { | ariaAttrs } -> Props eff st
aria v _ = P.aria v

_data :: ∀ dataAttrs eff st. { | dataAttrs } -> Props eff st
_data v _ = P._data v

style :: ∀ style eff st. { | style } -> Props eff st
style v _ = P.style v

dangerouslySetInnerHTML :: ∀ eff st. { __html :: String } -> Props eff st
dangerouslySetInnerHTML v _ = P.dangerouslySetInnerHTML v

accept :: ∀ eff st. String -> Props eff st
accept v _ = P.accept v

acceptCharset :: ∀ eff st. String -> Props eff st
acceptCharset v _ = P.acceptCharset v

accessKey :: ∀ eff st. String -> Props eff st
accessKey v _ = P.accessKey v

action :: ∀ eff st. String -> Props eff st
action v _ = P.action v

allowFullScreen :: ∀ eff st. Boolean -> Props eff st
allowFullScreen v _ = P.allowFullScreen v

allowTransparency :: ∀ eff st. Boolean -> Props eff st
allowTransparency v _ = P.allowTransparency v

alt :: ∀ eff st. String -> Props eff st
alt v _ = P.alt v

async :: ∀ eff st. Boolean -> Props eff st
async v _ = P.async v

autoComplete :: ∀ eff st. String -> Props eff st
autoComplete v _ = P.autoComplete v

autoFocus :: ∀ eff st. Boolean -> Props eff st
autoFocus v _ = P.autoFocus v

autoPlay :: ∀ eff st. Boolean -> Props eff st
autoPlay v _ = P.autoPlay v

capture :: ∀ eff st. Boolean -> Props eff st
capture v _ = P.capture v

cellPadding :: ∀ eff st. String -> Props eff st
cellPadding v _ = P.cellPadding v

cellSpacing :: ∀ eff st. String -> Props eff st
cellSpacing v _ = P.cellSpacing v

charSet :: ∀ eff st. String -> Props eff st
charSet v _ = P.charSet v

challenge :: ∀ eff st. String -> Props eff st
challenge v _ = P.challenge v

checked :: ∀ eff st. Boolean -> Props eff st
checked v _ = P.checked v

cite :: ∀ eff st. String -> Props eff st
cite v _ = P.cite v

classID :: ∀ eff st. String -> Props eff st
classID v _ = P.classID v

className :: ∀ eff st. String -> Props eff st
className v _ = P.className v

cols :: ∀ eff st. Int -> Props eff st
cols v _ = P.cols v

colSpan :: ∀ eff st. Int -> Props eff st
colSpan v _ = P.colSpan v

content :: ∀ eff st. String -> Props eff st
content v _ = P.content v

contentEditable :: ∀ eff st. Boolean -> Props eff st
contentEditable v _ = P.contentEditable v

contextMenu :: ∀ eff st. String -> Props eff st
contextMenu v _ = P.contextMenu v

controls :: ∀ eff st. Boolean -> Props eff st
controls v _ = P.controls v

coords :: ∀ eff st. String -> Props eff st
coords v _ = P.coords v

crossOrigin :: ∀ eff st. String -> Props eff st
crossOrigin v _ = P.crossOrigin v

dateTime :: ∀ eff st. String -> Props eff st
dateTime v _ = P.dateTime v

default :: ∀ eff st. Boolean -> Props eff st
default v _ = P.default v

defaultChecked :: ∀ eff st. Boolean -> Props eff st
defaultChecked v _ = P.defaultChecked v

defaultValue :: ∀ eff st. String -> Props eff st
defaultValue v _ = P.defaultValue v

defer :: ∀ eff st. Boolean -> Props eff st
defer v _ = P.defer v

dir :: ∀ eff st. String -> Props eff st
dir v _ = P.dir v

disabled :: ∀ eff st. Boolean -> Props eff st
disabled v _ = P.disabled v

download :: ∀ eff st. String -> Props eff st
download v _ = P.download v

draggable :: ∀ eff st. Boolean -> Props eff st
draggable v _ = P.draggable v

encType :: ∀ eff st. String -> Props eff st
encType v _ = P.encType v

form :: ∀ eff st. String -> Props eff st
form v _ = P.form v

formAction :: ∀ eff st. String -> Props eff st
formAction v _ = P.formAction v

formEncType :: ∀ eff st. String -> Props eff st
formEncType v _ = P.formEncType v

formMethod :: ∀ eff st. String -> Props eff st
formMethod v _ = P.formMethod v

formNoValidate :: ∀ eff st. Boolean -> Props eff st
formNoValidate v _ = P.formNoValidate v

formTarget :: ∀ eff st. String -> Props eff st
formTarget v _ = P.formTarget v

frameBorder :: ∀ eff st. String -> Props eff st
frameBorder v _ = P.frameBorder v

headers :: ∀ eff st. String -> Props eff st
headers v _ = P.headers v

height :: ∀ eff st. String -> Props eff st
height v _ = P.height v

hidden :: ∀ eff st. Boolean -> Props eff st
hidden v _ = P.hidden v

high :: ∀ eff st. String -> Props eff st
high v _ = P.high v

href :: ∀ eff st. String -> Props eff st
href v _ = P.href v

hrefLang :: ∀ eff st. String -> Props eff st
hrefLang v _ = P.hrefLang v

htmlFor :: ∀ eff st. String -> Props eff st
htmlFor v _ = P.htmlFor v

httpEquiv :: ∀ eff st. String -> Props eff st
httpEquiv v _ = P.httpEquiv v

icon :: ∀ eff st. String -> Props eff st
icon v _ = P.icon v

_id :: ∀ eff st. String -> Props eff st
_id v _ = P._id v

inputMode :: ∀ eff st. String -> Props eff st
inputMode v _ = P.inputMode v

integrity :: ∀ eff st. String -> Props eff st
integrity v _ = P.integrity v

is :: ∀ eff st. String -> Props eff st
is v _ = P.is v

key :: ∀ eff st. String -> Props eff st
key v _ = P.key v

keyParams :: ∀ eff st. String -> Props eff st
keyParams v _ = P.keyParams v

keyType :: ∀ eff st. String -> Props eff st
keyType v _ = P.keyType v

kind :: ∀ eff st. String -> Props eff st
kind v _ = P.kind v

label :: ∀ eff st. String -> Props eff st
label v _ = P.label v

lang :: ∀ eff st. String -> Props eff st
lang v _ = P.lang v

list :: ∀ eff st. String -> Props eff st
list v _ = P.list v

loop :: ∀ eff st. Boolean -> Props eff st
loop v _ = P.loop v

low :: ∀ eff st. String -> Props eff st
low v _ = P.low v

manifest :: ∀ eff st. String -> Props eff st
manifest v _ = P.manifest v

marginHeight :: ∀ eff st. String -> Props eff st
marginHeight v _ = P.marginHeight v

marginWidth :: ∀ eff st. String -> Props eff st
marginWidth v _ = P.marginWidth v

max :: ∀ eff st. String -> Props eff st
max v _ = P.max v

maxLength :: ∀ eff st. String -> Props eff st
maxLength v _ = P.maxLength v

media :: ∀ eff st. String -> Props eff st
media v _ = P.media v

mediaGroup :: ∀ eff st. String -> Props eff st
mediaGroup v _ = P.mediaGroup v

method :: ∀ eff st. String -> Props eff st
method v _ = P.method v

min :: ∀ eff st. String -> Props eff st
min v _ = P.min v

minLength :: ∀ eff st. String -> Props eff st
minLength v _ = P.minLength v

multiple :: ∀ eff st. Boolean -> Props eff st
multiple v _ = P.multiple v

muted :: ∀ eff st. Boolean -> Props eff st
muted v _ = P.muted v

name :: ∀ eff st. String -> Props eff st
name v _ = P.name v

nonce :: ∀ eff st. String -> Props eff st
nonce v _ = P.nonce v

noValidate :: ∀ eff st. Boolean -> Props eff st
noValidate v _ = P.noValidate v

open :: ∀ eff st. Boolean -> Props eff st
open v _ = P.open v

optimum :: ∀ eff st. String -> Props eff st
optimum v _ = P.optimum v

pattern :: ∀ eff st. String -> Props eff st
pattern v _ = P.pattern v

placeholder :: ∀ eff st. String -> Props eff st
placeholder v _ = P.placeholder v

poster :: ∀ eff st. String -> Props eff st
poster v _ = P.poster v

preload :: ∀ eff st. String -> Props eff st
preload v _ = P.preload v

profile :: ∀ eff st. String -> Props eff st
profile v _ = P.profile v

radioGroup :: ∀ eff st. String -> Props eff st
radioGroup v _ = P.radioGroup v

readOnly :: ∀ eff st. Boolean -> Props eff st
readOnly v _ = P.readOnly v

rel :: ∀ eff st. String -> Props eff st
rel v _ = P.rel v

required :: ∀ eff st. Boolean -> Props eff st
required v _ = P.required v

reversed :: ∀ eff st. Boolean -> Props eff st
reversed v _ = P.reversed v

role :: ∀ eff st. String -> Props eff st
role v _ = P.role v

rows :: ∀ eff st. Int -> Props eff st
rows v _ = P.rows v

rowSpan :: ∀ eff st. Int -> Props eff st
rowSpan v _ = P.rowSpan v

sandbox :: ∀ eff st. String -> Props eff st
sandbox v _ = P.sandbox v

scope :: ∀ eff st. String -> Props eff st
scope v _ = P.scope v

scoped :: ∀ eff st. Boolean -> Props eff st
scoped v _ = P.scoped v

scrolling :: ∀ eff st. String -> Props eff st
scrolling v _ = P.scrolling v

seamless :: ∀ eff st. Boolean -> Props eff st
seamless v _ = P.seamless v

selected :: ∀ eff st. Boolean -> Props eff st
selected v _ = P.selected v

shape :: ∀ eff st. String -> Props eff st
shape v _ = P.shape v

size :: ∀ eff st. Int -> Props eff st
size v _ = P.size v

sizes :: ∀ eff st. String -> Props eff st
sizes v _ = P.sizes v

span :: ∀ eff st. Int -> Props eff st
span v _ = P.span v

spellCheck :: ∀ eff st. Boolean -> Props eff st
spellCheck v _ = P.spellCheck v

src :: ∀ eff st. String -> Props eff st
src v _ = P.src v

srcDoc :: ∀ eff st. String -> Props eff st
srcDoc v _ = P.srcDoc v

srcLang :: ∀ eff st. String -> Props eff st
srcLang v _ = P.srcLang v

srcSet :: ∀ eff st. String -> Props eff st
srcSet v _ = P.srcSet v

start :: ∀ eff st. Int -> Props eff st
start v _ = P.start v

step :: ∀ eff st. String -> Props eff st
step v _ = P.step v

summary :: ∀ eff st. String -> Props eff st
summary v _ = P.summary v

tabIndex :: ∀ eff st. Int -> Props eff st
tabIndex v _ = P.tabIndex v

target :: ∀ eff st. String -> Props eff st
target v _ = P.target v

title :: ∀ eff st. String -> Props eff st
title v _ = P.title v

_type :: ∀ eff st. String -> Props eff st
_type v _ = P._type v

useMap :: ∀ eff st. String -> Props eff st
useMap v _ = P.useMap v

value :: ∀ eff st. String -> Props eff st
value v _ = P.value v

width :: ∀ eff st. String -> Props eff st
width v _ = P.width v

wmode :: ∀ eff st. String -> Props eff st
wmode v _ = P.wmode v

wrap :: ∀ eff st. String -> Props eff st
wrap v _ = P.wrap v

-- RDFa Attributes
about :: ∀ eff st. String -> Props eff st
about v _ = P.about v

datatype :: ∀ eff st. String -> Props eff st
datatype v _ = P.datatype v

inlist :: ∀ eff st. String -> Props eff st
inlist v _ = P.inlist v

prefix :: ∀ eff st. String -> Props eff st
prefix v _ = P.prefix v

property :: ∀ eff st. String -> Props eff st
property v _ = P.property v

resource :: ∀ eff st. String -> Props eff st
resource v _ = P.resource v

typeof :: ∀ eff st. String -> Props eff st
typeof v _ = P.typeof v

vocab :: ∀ eff st. String -> Props eff st
vocab v _ = P.vocab v

-- Non-standard Attributes
autoCapitalize :: ∀ eff st. String -> Props eff st
autoCapitalize v _ = P.autoCapitalize v

autoCorrect :: ∀ eff st. String -> Props eff st
autoCorrect v _ = P.autoCorrect v

autoSave :: ∀ eff st. String -> Props eff st
autoSave v _ = P.autoSave v

color :: ∀ eff st. String -> Props eff st
color v _ = P.color v

itemProp :: ∀ eff st. String -> Props eff st
itemProp v _ = P.itemProp v

itemScope :: ∀ eff st. Boolean -> Props eff st
itemScope v _ = P.itemScope v

itemType :: ∀ eff st. String -> Props eff st
itemType v _ = P.itemType v

itemID :: ∀ eff st. String -> Props eff st
itemID v _ = P.itemID v

itemRef :: ∀ eff st. String -> Props eff st
itemRef v _ = P.itemRef v

results :: ∀ eff st. Int -> Props eff st
results v _ = P.results v

security :: ∀ eff st. String -> Props eff st
security v _ = P.security v

unselectable :: ∀ eff st. Boolean -> Props eff st
unselectable v _ = P.unselectable v

--------------------------------------------------------------------------------

onAnimationStart :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onAnimationStart f effect = P.onAnimationStart (effect ○ f)

onAnimationEnd :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onAnimationEnd f effect = P.onAnimationEnd (effect ○ f)

onAnimationIteration :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onAnimationIteration f effect = P.onAnimationIteration (effect ○ f)

onTransitionEnd :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onTransitionEnd f effect = P.onTransitionEnd (effect ○ f)

onLoad :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onLoad f effect = P.onLoad (effect ○ f)

onCopy :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onCopy f effect = P.onCopy (effect ○ f)

onCut :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onCut f effect = P.onCut (effect ○ f)

onPaste :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onPaste f effect = P.onPaste (effect ○ f)

onKeyDown :: ∀ eff st.
  (KeyboardEvent -> Effect eff st Unit) -> Props eff st
onKeyDown f effect = P.onKeyDown (effect ○ f)

onKeyPress :: ∀ eff st.
  (KeyboardEvent -> Effect eff st Unit) -> Props eff st
onKeyPress f effect = P.onKeyPress (effect ○ f)

onKeyUp :: ∀ eff st.
  (KeyboardEvent -> Effect eff st Unit) -> Props eff st
onKeyUp f effect = P.onKeyUp (effect ○ f)

onFocus :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onFocus f effect = P.onFocus (effect ○ f)

onBlur :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onBlur f effect = P.onBlur (effect ○ f)

onChange :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onChange f effect = P.onChange (effect ○ f)

onInput :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onInput f effect = P.onInput (effect ○ f)

onInvalid :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onInvalid f effect = P.onInvalid (effect ○ f)

onSubmit :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onSubmit f effect = P.onSubmit (effect ○ f)

onClick :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onClick f effect = P.onClick (effect ○ f)

onDoubleClick :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onDoubleClick f effect = P.onDoubleClick (effect ○ f)

onDrag :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onDrag f effect = P.onDrag (effect ○ f)

onDragEnd :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onDragEnd f effect = P.onDragEnd (effect ○ f)

onDragEnter :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onDragEnter f effect = P.onDragEnter (effect ○ f)

onDragExit :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onDragExit f effect = P.onDragExit (effect ○ f)

onDragLeave :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onDragLeave f effect = P.onDragLeave (effect ○ f)

onDragOver :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onDragOver f effect = P.onDragOver (effect ○ f)

onDragStart :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onDragStart f effect = P.onDragStart (effect ○ f)

onDrop :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onDrop f effect = P.onDrop (effect ○ f)

onMouseDown :: ∀ eff st.
  (MouseEvent -> Effect eff st Unit) -> Props eff st
onMouseDown f effect = P.onMouseDown (effect ○ f)

onMouseEnter :: ∀ eff st.
  (MouseEvent -> Effect eff st Unit) -> Props eff st
onMouseEnter f effect = P.onMouseEnter (effect ○ f)

onMouseLeave :: ∀ eff st.
  (MouseEvent -> Effect eff st Unit) -> Props eff st
onMouseLeave f effect = P.onMouseLeave (effect ○ f)

onMouseMove :: ∀ eff st.
  (MouseEvent -> Effect eff st Unit) -> Props eff st
onMouseMove f effect = P.onMouseMove (effect ○ f)

onMouseOut :: ∀ eff st.
  (MouseEvent -> Effect eff st Unit) -> Props eff st
onMouseOut f effect = P.onMouseOut (effect ○ f)

onMouseOver :: ∀ eff st.
  (MouseEvent -> Effect eff st Unit) -> Props eff st
onMouseOver f effect = P.onMouseOver (effect ○ f)

onMouseUp :: ∀ eff st.
  (MouseEvent -> Effect eff st Unit) -> Props eff st
onMouseUp f effect = P.onMouseUp (effect ○ f)

onTouchCancel :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onTouchCancel f effect = P.onTouchCancel (effect ○ f)

onTouchEnd :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onTouchEnd f effect = P.onTouchEnd (effect ○ f)

onTouchMove :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onTouchMove f effect = P.onTouchMove (effect ○ f)

onTouchStart :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onTouchStart f effect = P.onTouchStart (effect ○ f)

onScroll :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onScroll f effect = P.onScroll (effect ○ f)

onWheel :: ∀ eff st.
  (Event -> Effect eff st Unit) -> Props eff st
onWheel f effect = P.onWheel (effect ○ f)

--------------------------------------------------------------------------------

suppressContentEditableWarning :: ∀ eff st. Boolean -> Props eff st
suppressContentEditableWarning v _ = P.suppressContentEditableWarning v

-- SVG attributes
x :: ∀ eff st. Int -> Props eff st
x v _ = P.x v

y :: ∀ eff st. Int -> Props eff st
y v _ = P.y v

cx :: ∀ eff st. Int -> Props eff st
cx v _ = P.cx v

cy :: ∀ eff st. Int -> Props eff st
cy v _ = P.cy v

r :: ∀ eff st. Int -> Props eff st
r v _ = P.r v

fill :: ∀ eff st. String -> Props eff st
fill v _ = P.fill v

opacity :: ∀ eff st. Int -> Props eff st
opacity v _ = P.opacity v

fillOpacity :: ∀ eff st. Int -> Props eff st
fillOpacity v _ = P.fillOpacity v

stroke :: ∀ eff st. String -> Props eff st
stroke v _ = P.stroke v

strokeWidth :: ∀ eff st. Int -> Props eff st
strokeWidth v _ = P.strokeWidth v

points :: ∀ eff st. String -> Props eff st
points v _ = P.points v

d :: ∀ eff st. String -> Props eff st
d v _ = P.d v

viewBox :: ∀ eff st. String -> Props eff st
viewBox v _ = P.viewBox v

--------------------------------------------------------------------------------

unsafeEventConvert :: ∀ a. Event -> a
unsafeEventConvert = unsafeCoerce

onEnter :: ∀ eff st. Effect eff st Unit -> Props eff st
onEnter f effect = P.onKeyDown \e -> effect do
  if e.keyCode == 13 then f else pure unit

onEscape :: ∀ eff st. Effect eff st Unit -> Props eff st
onEscape f effect = P.onKeyDown \e -> effect do
  if e.keyCode == 27 then f else pure unit
