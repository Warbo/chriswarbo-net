<xsl:stylesheet
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:m="http://www.w3.org/1998/Math/MathML"
  exclude-result-prefixes="m"
  >
  <xsl:output method="xml" />

  <!-- Recurse through elements -->

  <xsl:template match="/">
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>

  <!-- Present applications of unary_minus as overbar -->
  <xsl:template
      match="m:apply[*[1][self::m:minus] and count(*)=2]
             |m:apply[*[1][self::m:csymbol='unary_minus']]">
    <m:mover>
      <xsl:apply-templates select="*[2]"/>
      <m:mo>&#175;<!-- overline --></m:mo>
    </m:mover>
  </xsl:template>

</xsl:stylesheet>
