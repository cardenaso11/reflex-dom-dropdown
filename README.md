reflex-dom-popup
===============

Not actually a dropdown specific, but a popup. Use anywhere you normally use popups.

Usage
-----

Below is an example of using the popup, toggled by a simple button.

```haskell

> {-# Language OverloadedStrings #-}
> {-# Language RecursiveDo #-}
> {-# Language TypeApplications #-}
> import Reflex.Dom
> import Reflex.Dom.Attrs
> import Reflex.Dom.Dropdown
> import Reflex.Dom.Tables
> import qualified Data.Map as Map
> import qualified Data.Text as T
> import Control.Monad (void)

> main :: IO ()
> main = mainWidgetWithCss extraStyle etc


```


Hacking
-------

To work on this library, enter the nix shell with the following command:

```bash
nix-shell -A project.haskell-nix
```

Once you're inside that shell, you can use `cabal repl` for quick feedback
while developing. To build and test your changes, run:

```bash
javascript-unknown-ghcjs-cabal build
```

You'll see some output like this:

```
dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/reflex-dom-lazy-0.1.0.0/x/reflex-dom-lazy/build/reflex-dom-lazy/reflex-dom-lazy.jsexe
```

Open `index.html` at that path to run the code from `Readme.lhs` in your browser.
