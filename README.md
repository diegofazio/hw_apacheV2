# hw_apacheV2
HW_ApacheV2 - Harbour Apache standalone Module

Add these lines at the bottom of httpd.conf

```
WINDOWS
LoadModule harbourV2_module modules/hw_apacheV2.so

LINUX
LoadModule harbourV2_module /usr/lib/apache2/modules/hw_apacheV2.so

<FilesMatch "\.(prg|hrb)$">
    SetHandler harbour
</FilesMatch>

```
