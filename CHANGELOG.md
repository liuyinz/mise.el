# Changelog

## [0.3.0](https://github.com/liuyinz/mise.el/compare/v0.2.0..v0.3.0) - 2024-07-07

### Bug Fixes

- solve #1, do not start mise-mode if parent directory does not exit - ([f94860f](https://github.com/liuyinz/mise.el/commit/f94860f7e3b6fdaea7eb79beff8f4662f137752b))
- delay mise--update when mise is enabled to run default mode hook first - ([92c24f0](https://github.com/liuyinz/mise.el/commit/92c24f06d8d47028ea30ba1c27808ed638372c12))

### Refactoring

- use -each instead - ([3044e22](https://github.com/liuyinz/mise.el/commit/3044e2217136885ebf1b41f478ba1ebf6fc770b6))
- update experimental feature condition - ([dcb1487](https://github.com/liuyinz/mise.el/commit/dcb1487ffd45ea51d00fdccd0c49be5dc2830dc1))
- use mise--message function to send message instead - ([36250ac](https://github.com/liuyinz/mise.el/commit/36250acaf2a2f5f46c18f4121369a5641b0ab766))
- mise debug info style - ([34a3450](https://github.com/liuyinz/mise.el/commit/34a34505ec72d984329881884ed05311cc0aa133))
- remove unused var - ([39c4cb8](https://github.com/liuyinz/mise.el/commit/39c4cb8639d9dd3382954680f62846fbb551e44c))

## [0.2.0](https://github.com/liuyinz/mise.el/compare/v0.1.0..v0.2.0) - 2024-05-26

### Bug Fixes

- typos in let binding - ([cb19405](https://github.com/liuyinz/mise.el/commit/cb19405eba3cfd679a5e3ac75c8c4c1146b16015))

### Refactoring

- add function mise--ensure to prepared mise-mode if non-nil - ([9d743e0](https://github.com/liuyinz/mise.el/commit/9d743e0c14a97fd613f39cad3a6c26ce9eb076e6))

## [0.1.0] - 2024-05-14

### Miscellaneous Chores

- add cliff.toml - ([0327ccc](https://github.com/liuyinz/mise.el/commit/0327ccca666a311aab9a9024f48e21c1b222da21))

### Refactoring

- use dash lib instead of seq - ([90f31ac](https://github.com/liuyinz/mise.el/commit/90f31ace0048e6615b1db944ba14871fd0da0016))

<!-- generated by git-cliff -->
