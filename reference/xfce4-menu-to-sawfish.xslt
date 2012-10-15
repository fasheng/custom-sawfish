<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
  <xsl:strip-space elements="*"/>
  <xsl:param name="terminal">urxvt</xsl:param>
  <xsl:template name="indent">
    <xsl:text>
</xsl:text>
    <xsl:for-each select="ancestor-or-self::*">
      <xsl:text>  </xsl:text>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="/">
    <xsl:text>;;; Arch applications menu for Sawfish, generated from xfce4 menu
(defvar arch-menu nil)
(setq arch-menu '(</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>))
</xsl:text>
  </xsl:template>
  <xsl:template match="menu[.//app]">
    <xsl:call-template name="indent"/>
    <xsl:text>("</xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text>"</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>)</xsl:text>
  </xsl:template>
  <xsl:template match="app">
    <xsl:call-template name="indent"/>
    <xsl:text>("</xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text>" (system "</xsl:text>
    <xsl:if test="@term='true'">
      <xsl:copy-of select="$terminal"/>
      <xsl:text> -e </xsl:text>
    </xsl:if>
    <xsl:value-of select="@cmd"/>
    <xsl:text> "))</xsl:text>
  </xsl:template>
</xsl:stylesheet>
