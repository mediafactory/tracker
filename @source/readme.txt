to build you need:
- Lazarus
- lNet >= 0.65
- SemaphoreGrid => shipped with lazarus
- XMLRPC lib => shipped inside @source\open_xmlrpc
- openURL function, that did it not make yet ito lazarus / LCL => shipped inside @source\open_etc

to use:

WITH YATS:
- you need a YATS installation
- works out of the box :-)

WITH TRAC:
- you need a TRAC installation :-)
- you need the XMLRPC plugin
- you need to set rights for XMLRPC and admin...
- enter URL: http://my.trac.installation/login/xmlrpc => note that /login/xmlrpc is required for the XMLRPC plugin to work