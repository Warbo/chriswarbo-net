<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:ext="http://exslt.org/common"
                xmlns:html="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="xs ext">

  <!--
  Prefix absolute URLs, e.g. "/blog/foo.html", with a relative path, e.g
  "../blog/foo.html". Takes a parameter "to_root", which can be specified via
  xsltproc's "stringparam" argument.

  Adapted from http://stackoverflow.com/q/37838126/884682
  -->

  <xsl:output method="html" encoding="UTF-8" omit-xml-declaration="yes"
              doctype-system="about:legacy-compat" indent="no"/>

  <xsl:param name="to_root" />

  <xsl:template match="@* | node()">
    <xsl:copy>
      <xsl:apply-templates select="@* | node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='html:a/@href[starts-with(.,"/")]'>
    <xsl:attribute name="href">
      <xsl:value-of select="concat($to_root,.)"/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match='html:link/@href[starts-with(.,"/")]'>
    <xsl:attribute name="href">
      <xsl:value-of select="concat($to_root,.)"/>
    </xsl:attribute>
  </xsl:template>
</xsl:stylesheet>
