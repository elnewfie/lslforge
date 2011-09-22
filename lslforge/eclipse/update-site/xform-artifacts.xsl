<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:param name="baseUrl">http://localhost:8080/bar</xsl:param>
<xsl:variable name="idtxt">${id}</xsl:variable>
<xsl:variable name="vertxt">${version}</xsl:variable>
<xsl:template match="mappings">
  <mappings size='3'>
    <rule filter='(&amp; (classifier=osgi.bundle))' output='{$baseUrl}/{$idtxt}_plugin_{$vertxt}.jar'/>
    <rule filter="(&amp; (classifier=binary))" output="{$baseUrl}/{$idtxt}_{$vertxt}"/>
    <rule filter='(&amp; (classifier=org.eclipse.update.feature))' output='{$baseUrl}/{$idtxt}_feature_{$vertxt}.jar'/>
  </mappings>
</xsl:template>
<xsl:template match="@*|node()">
   <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
   </xsl:copy>
</xsl:template>
</xsl:stylesheet>
