---
title: How's This For a Mention?
dependencies: [ 'static/file2img.sh', 'static/images/ILoveJo.png' ]
---
To Jo

```{.unwrap pipe="sh | pandoc -t json"}
./root/static/file2img.sh "" < ./root/static/images/ILoveJo.png
```
