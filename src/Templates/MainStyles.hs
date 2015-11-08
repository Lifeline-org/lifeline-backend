{-# LANGUAGE
    TemplateHaskell
  , QuasiQuotes
  , OverloadedStrings
  #-}

module Templates.MainStyles where

import Text.Lucius
import qualified Data.Text.Lazy as LT

mainStyles :: LT.Text
mainStyles = renderCss $ mainStyles' undefined
  where
  mainStyles' = [lucius|

html, body
{
        padding: 0;
        margin: 0;
}
body
{
        background-color: #fff;
        font-family: Arial, Helvetica, Verdana;
        font-size: 14px;
        line-height: 22px;
        color: #666;
        position: relative;
        -webkit-text-size-adjust: none;
}
body *
{
        text-shadow: none;
}
h1, h2, h3, h4, h5, h6
{
        line-height: 1;
        font-weight: bold;
        margin: 20px 0 10px 0;
}
h1, h2, h3
{
        font-size: 18px;
}
h4, h5, h6
{
        font-size: 16px;
}
p
{
        margin: 0 0 10px 0;
}
a, a:link, a:active, a:visited, a:hover
{
        color: inherit;
        text-decoration: underline;
}

nav:not(.mm-menu)
{
        display: none;
}

.header,
.footer
{
        text-align: center;
}
.header,
.footer
{
        background: #7f003f;
        font-size: 16px;
        font-weight: bold;
        color: #fff;
        line-height: 40px;


        -moz-box-sizing: border-box;
        box-sizing: border-box;
        width: 100%;
        height: 40px;
        padding: 0 50px;
}
.header.fixed
{
        position: fixed;
        top: 0;
        left: 0;
}
.footer.fixed
{
        position: fixed;
        bottom: 0;
        left: 0;
}
.header a
{
        background: center center no-repeat transparent;
        background-image: url( data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABkAAAAZCAYAAADE6YVjAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAADhJREFUeNpi/P//PwOtARMDHQBdLGFBYtMq3BiHT3DRPU4YR4NrNAmPJuHRJDyahEeT8Ii3BCDAAF0WBj5Er5idAAAAAElFTkSuQmCC );

        display: block;
        width: 40px;
        height: 40px;
        position: absolute;
        top: 0;
        left: 10px;
}

.content
{
        padding: 910px 0px 0px 0px;
}

#intro,
#intro,
#first,
#second,
#third
{
        height: 400px;
}
#intro
{
        padding-top: 0;
}
#first,
#second,
#third
{
        border-top: 1px solid #ccc;
        padding-top: 150px;
}

.header,
.footer
{
        box-sizing: border-box;
        width: 100%;
        position: fixed;
}
.header
{
        top: 0;
}
.footer
{
        bottom: 0;
}

#map-canvas {
        /*margin-top:175px;
        width:80%;
        height:500px;*/

        position: absolute;
        height: auto;
        bottom: 0;
        top: 0;
        left: 0;
        right: 0;
        margin-top:20px;
}

.findIssuesFilter {
        display: none;
}

.reportIssuesFilter {
        display: none;
}

#legend {
        background: #black;
        padding: 20px;
        /*display: none;*/
}

#legend li {
        font-size: 1.3em;
        font-weight: bold;
}

#greenColor {
        color: #2AA42A;
}

#yellowColor {
        color: #ADAD59;
}

#pinkColor {
        color: #A9366F;
}

#purpleColor {
        color: #835FA8;
}

#menu li {
        margin-bottom: 20px;
        text-align: center;
}

.clear{
        clear: both;
        padding-top: 10px;
}

  |]
