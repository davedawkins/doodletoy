## Sutil Template for Hello World

The simplest Sutil application. This gives you a development area and shows how a single page application is constructed and mounted with minimal styling. See
- src/App/App.fs.
- public/index.html

```fs
module App

open Sutil
open Sutil.DOM
open Sutil.Attr

let view() =
    Html.div [
        style [
            Css.fontFamily "Arial, Helvetica,sans-serif"
            Css.textAlign "center"
            Css.marginTop "40px"
            Css.fontSize "10ex"
        ]
        text "Hello World"
    ]

mountElement "sutil-app" (view())
```

### Quick Start

```
    git clone -s https://github.com/davedawkins/sutil-template-helloworld.git
    cd sutil-template-helloworld
    dotnet tool restore
    npm install
    npm run start
```

### What Next
It's recommended that for any kind of "real" app that you adopt the Elmish MVU pattern.
Template: https://github.com/davedawkins/Sutil-template-elmish
