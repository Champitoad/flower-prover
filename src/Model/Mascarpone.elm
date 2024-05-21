module Model.Mascarpone exposing (..)

import Model.Formula exposing (..)
import Model.Flower exposing (..)

mascarpone : Formula
mascarpone =
  Atom (Image
  { src = "https://cdn.iconscout.com/icon/premium/png-256-thumb/mascarpone-8021586-6817680.png?f=webp"
  , description = "mascarpone" })

sugar : Formula
sugar =
  Atom (Image
  { src = "https://cdn-icons-png.flaticon.com/256/5900/5900648.png"
  , description = "sugar" })


egg : Formula
egg =
  Atom (Image
  { src = "https://icons.iconarchive.com/icons/google/noto-emoji-food-drink/256/32390-egg-icon.png"
  , description = "egg" })

white : Formula
white =
  Atom (Image
  { src = "https://images.eatthismuch.com/img/104_ldementhon_c4a1f6c1-fd55-4433-a3d1-9f2f927c2426.png"
  , description = "white" })

yolk : Formula
yolk =
  Atom (Image
  { src = "https://images.eatthismuch.com/img/105_erin_m_38ebb126-d5cc-406a-be89-860441cb8cb4.png"
  , description = "yolk" })

whiskedWhites : Formula
whiskedWhites =
  Atom (Image
  { src = "https://cdn.apartmenttherapy.info/image/upload/f_jpg,q_auto:eco,c_fill,g_auto,w_1500,ar_1:1/k%2Farchive%2F269de951aa95bc2b31c971ab0be50822e12d90b4"
  , description = "whisked whites" })

yolkyPaste : Formula
yolkyPaste =
  Atom (Image
  { src = "https://www.peteandgerrys.com/cdn/shop/articles/egg-yolk-sauce-web.jpg?v=1681295675"
  , description = "yolky paste" })

thickPaste : Formula
thickPaste =
  Atom (Image
  { src = "https://stressbaking.com/wp-content/uploads/2015/07/mascarpone-whipped-cream-square.jpg"
  , description = "thick paste" })

mascarponeCream : Formula
mascarponeCream =
  Atom (Image
  { src = "https://anitalianinmykitchen.com/wp-content/uploads/2020/02/mascarpone-cream-sq-1-of-1.jpg"
  , description = "mascarpone cream" })

crack : Flower
crack =
  entails [f egg] [f yolk, f white]

whisk : Flower
whisk =
  entails [f white] [f whiskedWhites]

beat : Flower
beat =
  entails [f yolk, f sugar] [f yolkyPaste]

stir : Flower
stir =
  entails [f yolkyPaste, f mascarpone] [f thickPaste]

fold : Flower
fold =
  entails [f whiskedWhites, f thickPaste] [f mascarponeCream]