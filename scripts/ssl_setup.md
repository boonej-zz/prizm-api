SSL Creation/Setup command reminders
====================================

`these steps are not ment to follow exactly; more of an outline of what you need to do`

make sure to install openssl -- sudo yum -y install openssl

Create Cert Authority
====================
openssl genrsa -des3 -out ca.key 1024
openssl req -new -key ca.key -out ca.csr
openssl x509 -req -days 365 -in ca.csr -out ca.crt -signkey ca.key

Create a Server Cert
====================
openssl genrsa -des3 -out server.key 1024
openssl req -new -key server.key -out server.csr

Remove Passphrase from server cert
=================================
`remove passphrase from the server cert OR you will get a password error`
cp server.key server.key.org
openssl rsa -in server.key.org -out server.key

Generate self-signed Cert
=========================
openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt

