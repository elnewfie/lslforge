<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:param name="desc"></xsl:param>
<xsl:template match="@name[parent::repository]"><xsl:attribute name="name"><xsl:copy-of select="$desc"/></xsl:attribute></xsl:template>
<xsl:template match="@*|node()">
   <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
   </xsl:copy>
</xsl:template>
</xsl:stylesheet>
