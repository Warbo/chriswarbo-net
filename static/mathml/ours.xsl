<xsl:stylesheet
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:m="http://www.w3.org/1998/Math/MathML"
  exclude-result-prefixes="m"
  >
  <xsl:output method="xml" />

  <!-- Recurse through elements -->

  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <!-- Always represent division using mfrac -->
  <xsl:template match="m:apply[*[1][self::m:divide]]
                       |m:apply[*[1][self::m:csymbol='divide']]">
    <m:mfrac>
      <xsl:apply-templates select="*[2]"/>
      <xsl:apply-templates select="*[3]"/>
    </m:mfrac>
  </xsl:template>


  <!-- Summands beginning with a minus need parentheses -->
  <xsl:template
      match="m:mrow[*[1][self::m:mo][text()='−']][preceding-sibling::m:mo[text()='+']]">
    <m:mrow>
      <m:mo>(</m:mo>
      <m:mfenced>
        <xsl:apply-templates/>
      </m:mfenced>
      <m:mo>)</m:mo>
    </m:mrow>
  </xsl:template>

  <!-- When multiplying literal numbers, they must be separated by an explicit ×
       rather than an "invisible times". The CTOP stylesheet handles 'unadorned'
       literals, but not those decorated with superscripts, overlines, etc. -->
  <xsl:template
      match="m:mo[text()='⁢'][preceding-sibling::m:mn][
             following-sibling::m:msup[*[1][self::m:mn]]
             ]">
    <!-- Writes '3×10²' instead of '3⁢10²' -->
    <m:mo>×</m:mo>
  </xsl:template>
  <xsl:template
      match="m:mo[text()='⁢'][
             preceding-sibling::m:mover[*[1][self::m:mn]]
             ][
             following-sibling::m:msup[*[1][self::m:mn]]
             ]">
    <!-- Writes 3̄×10² instead of 3̄⁢10²-->
    <m:mo>×</m:mo>
  </xsl:template>

</xsl:stylesheet>
