# helm-lines.el

A helm interface for completing by lines elsewhere in your project.

[![MELPA](https://melpa.org/packages/helm-lines-badge.svg)](https://melpa.org/#/helm-lines)

## Installing

It's Soon™ on [MELPA](https://melpa.org/#/getting-started). When it is, run `M-x package-install helm-links` to install it.

Or preferably using the ingenious [use-package](https://github.com/jwiegley/use-package) declaration

```
(use-package helm-lines
  :commands (helm-lines))
```

## Features

This plugin will let you complete lines you already typed elsewhere in your project, by searching through files in the current git repo using [`ag`](https://github.com/ggreer/the_silver_searcher), displaying the results in helm.

Run `helm-lines` to pop open a search for a line that contains what you already typed on the line you are at. Skip between them with `C-p` and `C-n` and hit enter to finish up the line. 

<img src="https://github.com/torgeir/helm-lines.el/blob/master/demos/helm-lines.gif?raw=true" width="80%" alt="helm-lines-feature-preview" />

Use it with `helm-follow-mode` to show the resulting line in-place.

## Credits/inspiration

helm-ag - https://github.com/syohex/emacs-helm-ag

## License

Copyright (C) 2018 Torgeir Thoresen

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
