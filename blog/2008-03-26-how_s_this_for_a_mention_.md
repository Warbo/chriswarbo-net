---
title: How's This For a Mention?
dependencies: [ 'static/images/ILoveJo.png' ]
packages: [ 'file2img' ]
---
To Jo

```{.unwrap pipe="sh | pandoc -t json"}
file2img "" < ./root/static/images/ILoveJo.png
```
