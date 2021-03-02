<?xml version="1.0" encoding="UTF-8"?>
<!-- This file is based on the Toscana2/3 example PCTest20/PCTest80 and has been converted by Toscana 3,
	afterwards the diagrams have been scaled to acommodate with the fact that nodes and lines now have
	dimension in the model coordinate system.
	
	Here are the parameters to scale the diagrams from the original CSC->CSX conversion to this file:
	
        <xsl:param name="diagram">Leistung des Netzteils und Zahl der Anschlüsse</xsl:param>
        <xsl:param name="scale">3</xsl:param>
        <xsl:param name="diagram">Graphikkarte</xsl:param>
        <xsl:param name="scale">3.5</xsl:param>
        <xsl:param name="diagram">Zugängliche Laufwerke</xsl:param>
        <xsl:param name="scale">5</xsl:param>
        <xsl:param name="diagram">Bustypen der 486/66 PCs</xsl:param>
        <xsl:param name="scale">4</xsl:param>
        <xsl:param name="diagram">WinMarks (Graphics/Disk) für 486/66 PCs</xsl:param>
        <xsl:param name="scale">4</xsl:param>
        <xsl:param name="diagram">Festplattengrößen (ordinal)</xsl:param>
        <xsl:param name="scale">5</xsl:param>
        <xsl:param name="diagram">DOS-Win-Mark</xsl:param>
        <xsl:param name="scale">4</xsl:param>
        <xsl:param name="diagram">Vertriebsform</xsl:param>
        <xsl:param name="scale">7</xsl:param>
        <xsl:param name="diagram">Gehäusetyp</xsl:param>
        <xsl:param name="scale">2.5</xsl:param>
        <xsl:param name="diagram">Ports auf dem Motherboard</xsl:param>
        <xsl:param name="scale">3</xsl:param>
        <xsl:param name="diagram">interne Laufwerksschächte</xsl:param>
        <xsl:param name="scale">4</xsl:param>
-->
<conceptualSchema version="TJ1.0">
    <diagram title="Preise für 486/66 PCs">
        <node id="1">
            <position x="0.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="2">
            <position x="20.0" y="20.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=2500$</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="40.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=3000$</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="4">
            <position x="60.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=3500$</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="5">
            <position x="80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=4000$</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="6">
            <position x="100.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=4500$</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="7">
            <position x="120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Compaq Deskpro 66M</object>
                    <object>IBM PS/2 Model 77 486DX2</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&gt;=5000$</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="8">
            <position x="-20.0" y="20.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;5000$</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="9">
            <position x="0.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="10">
            <position x="20.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="11">
            <position x="40.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="12">
            <position x="60.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="13">
            <position x="80.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>FutureTech System 462E</object>
                    <object>HP Vectra 486/66U</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>NCR System 3350</object>
                    <object>NEC Express DX2/66e</object>
                    <object>Swan 486DX2-66DB</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="14">
            <position x="-40.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;4500$</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="15">
            <position x="-20.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="16">
            <position x="0.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="17">
            <position x="20.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="18">
            <position x="40.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>AST Bravo 4/66d</object>
                    <object>Arche Legacy 486/66DX2</object>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>Ariel 486DX2-66VLB</object>
                    <object>BOSS 466d</object>
                    <object>CompuAdd 466E</object>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>Dell 466DE/2</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="19">
            <position x="-60.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;4000$</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="20">
            <position x="-40.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="21">
            <position x="-20.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="22">
            <position x="0.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>American Super Computer 486X2/e66</object>
                    <object>DFI 486-66DX2</object>
                    <object>FCS 486-66</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>Mega Impact 486DX2/66E+</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                    <object>Osicom i466 MOD 420</object>
                    <object>PCS Double Pro-66</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>Tangent Model 466ex</object>
                    <object>Tri-Star 66/DX2-VL</object>
                    <object>USA Flex 486DX2/66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="23">
            <position x="-80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;3500$</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="24">
            <position x="-60.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="25">
            <position x="-40.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ATronics ATI-486-66</object>
                    <object>American Mitac TL4466</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>Edge 466 Magnum</object>
                    <object>Gecco 466E</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                    <object>Lightning ThunderBox</object>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>Naga Windows Workstation</object>
                    <object>Northgate SlimLine ZXP</object>
                    <object>Poly 486-66LM</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                    <object>Wyse Decision 486si</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="26">
            <position x="-100.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;3000$</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="27">
            <position x="-80.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Austin 466DX2 WinStation</object>
                    <object>BLK 486DX2/66</object>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>Blue Star 466D2U</object>
                    <object>Comex 486DX2/66</object>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>Diamond DX2-66</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>Expo 486 dX2/66</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>Hyundai 466D2</object>
                    <object>IDS 466i2</object>
                    <object>Insight 486DX2-66I</object>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                    <object>Occidental 66MHz 486DX2</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                    <object>Twinhead Superset 600/462D</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="28">
            <position x="-120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Broadax 486DX2-66</object>
                    <object>CAF Gold 6D2</object>
                    <object>NETiS Ultra WinStation N466L</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&lt;2500$</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="29">
            <position x="0.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="1" to="8" />
        <edge from="2" to="3" />
        <edge from="2" to="9" />
        <edge from="3" to="4" />
        <edge from="3" to="10" />
        <edge from="4" to="5" />
        <edge from="4" to="11" />
        <edge from="5" to="6" />
        <edge from="5" to="12" />
        <edge from="6" to="7" />
        <edge from="6" to="13" />
        <edge from="7" to="29" />
        <edge from="8" to="9" />
        <edge from="8" to="14" />
        <edge from="9" to="10" />
        <edge from="9" to="15" />
        <edge from="10" to="11" />
        <edge from="10" to="16" />
        <edge from="11" to="12" />
        <edge from="11" to="17" />
        <edge from="12" to="13" />
        <edge from="12" to="18" />
        <edge from="13" to="29" />
        <edge from="14" to="15" />
        <edge from="14" to="19" />
        <edge from="15" to="16" />
        <edge from="15" to="20" />
        <edge from="16" to="17" />
        <edge from="16" to="21" />
        <edge from="17" to="18" />
        <edge from="17" to="22" />
        <edge from="18" to="29" />
        <edge from="19" to="20" />
        <edge from="19" to="23" />
        <edge from="20" to="21" />
        <edge from="20" to="24" />
        <edge from="21" to="22" />
        <edge from="21" to="25" />
        <edge from="22" to="29" />
        <edge from="23" to="24" />
        <edge from="23" to="26" />
        <edge from="24" to="25" />
        <edge from="24" to="27" />
        <edge from="25" to="29" />
        <edge from="26" to="27" />
        <edge from="26" to="28" />
        <edge from="27" to="29" />
        <edge from="28" to="29" />
    </diagram>
    <diagram title="Video (1000 Operationen je Sekunde)">
        <node id="1">
            <position x="0.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="2">
            <position x="20.0" y="20.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=1500</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="40.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=3000</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="4">
            <position x="60.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=4500</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="5">
            <position x="80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=6000</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="6">
            <position x="100.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=7500</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="7">
            <position x="120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                    <object>NETiS Ultra WinStation N466L</object>
                    <object>Wyse Decision 486si</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&gt;=9000</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="8">
            <position x="-20.0" y="20.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;9000</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="9">
            <position x="0.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="10">
            <position x="20.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="11">
            <position x="40.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="12">
            <position x="60.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="13">
            <position x="80.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="14">
            <position x="-40.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;7500</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="15">
            <position x="-20.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="16">
            <position x="0.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="17">
            <position x="20.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="18">
            <position x="40.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ariel 486DX2-66VLB</object>
                    <object>Austin 466DX2 WinStation</object>
                    <object>Compaq Deskpro 66M</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>Hyundai 466D2</object>
                    <object>Lightning ThunderBox</object>
                    <object>Northgate SlimLine ZXP</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="19">
            <position x="-60.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;6000</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="20">
            <position x="-40.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="21">
            <position x="-20.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="22">
            <position x="0.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>Arche Legacy 486/66DX2</object>
                    <object>CompuAdd 466E</object>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>Insight 486DX2-66I</object>
                    <object>NCR System 3350</object>
                    <object>NEC Express DX2/66e</object>
                    <object>Occidental 66MHz 486DX2</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="23">
            <position x="-80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;4500</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="24">
            <position x="-60.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="25">
            <position x="-40.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>AST Bravo 4/66d</object>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>BOSS 466d</object>
                    <object>Blue Star 466D2U</object>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>DFI 486-66DX2</object>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>Gecco 466E</object>
                    <object>IBM PS/2 Model 77 486DX2</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                    <object>Naga Windows Workstation</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                    <object>Tangent Model 466ex</object>
                    <object>Twinhead Superset 600/462D</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="26">
            <position x="-100.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;3000</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="27">
            <position x="-80.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ATronics ATI-486-66</object>
                    <object>American Super Computer 486X2/e66</object>
                    <object>BLK 486DX2/66</object>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>Broadax 486DX2-66</object>
                    <object>CAF Gold 6D2</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>Comex 486DX2/66</object>
                    <object>Dell 466DE/2</object>
                    <object>Diamond DX2-66</object>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>Edge 466 Magnum</object>
                    <object>Expo 486 dX2/66</object>
                    <object>FCS 486-66</object>
                    <object>FutureTech System 462E</object>
                    <object>HP Vectra 486/66U</object>
                    <object>IDS 466i2</object>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>Mega Impact 486DX2/66E+</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                    <object>PCS Double Pro-66</object>
                    <object>Poly 486-66LM</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>Swan 486DX2-66DB</object>
                    <object>Tri-Star 66/DX2-VL</object>
                    <object>USA Flex 486DX2/66</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="28">
            <position x="-120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Mitac TL4466</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>Osicom i466 MOD 420</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&lt;1500</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="29">
            <position x="0.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="1" to="8" />
        <edge from="2" to="3" />
        <edge from="2" to="9" />
        <edge from="3" to="4" />
        <edge from="3" to="10" />
        <edge from="4" to="5" />
        <edge from="4" to="11" />
        <edge from="5" to="6" />
        <edge from="5" to="12" />
        <edge from="6" to="7" />
        <edge from="6" to="13" />
        <edge from="7" to="29" />
        <edge from="8" to="9" />
        <edge from="8" to="14" />
        <edge from="9" to="10" />
        <edge from="9" to="15" />
        <edge from="10" to="11" />
        <edge from="10" to="16" />
        <edge from="11" to="12" />
        <edge from="11" to="17" />
        <edge from="12" to="13" />
        <edge from="12" to="18" />
        <edge from="13" to="29" />
        <edge from="14" to="15" />
        <edge from="14" to="19" />
        <edge from="15" to="16" />
        <edge from="15" to="20" />
        <edge from="16" to="17" />
        <edge from="16" to="21" />
        <edge from="17" to="18" />
        <edge from="17" to="22" />
        <edge from="18" to="29" />
        <edge from="19" to="20" />
        <edge from="19" to="23" />
        <edge from="20" to="21" />
        <edge from="20" to="24" />
        <edge from="21" to="22" />
        <edge from="21" to="25" />
        <edge from="22" to="29" />
        <edge from="23" to="24" />
        <edge from="23" to="26" />
        <edge from="24" to="25" />
        <edge from="24" to="27" />
        <edge from="25" to="29" />
        <edge from="26" to="27" />
        <edge from="26" to="28" />
        <edge from="27" to="29" />
        <edge from="28" to="29" />
    </diagram>
    <diagram title="Festplattengröße von 486/66 PCs">
        <node id="1">
            <position x="0.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="2">
            <position x="20.0" y="20.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=225MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="40.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=250MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="4">
            <position x="60.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=350MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="5">
            <position x="80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=400MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="6">
            <position x="100.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=450MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="7">
            <position x="120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ariel 486DX2-66VLB</object>
                    <object>NEC Express DX2/66e</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&gt;=500MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="8">
            <position x="-20.0" y="20.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;500MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="9">
            <position x="0.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="10">
            <position x="20.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="11">
            <position x="40.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="12">
            <position x="60.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="13">
            <position x="80.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Insight 486DX2-66I</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="14">
            <position x="-40.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;450MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="15">
            <position x="-20.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="16">
            <position x="0.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="17">
            <position x="20.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="18">
            <position x="40.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>FutureTech System 462E</object>
                    <object>IBM PS/2 Model 77 486DX2</object>
                    <object>Osicom i466 MOD 420</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="19">
            <position x="-60.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;400MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="20">
            <position x="-40.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="21">
            <position x="-20.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="22">
            <position x="0.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Mitac TL4466</object>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>IDS 466i2</object>
                    <object>Lightning ThunderBox</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="23">
            <position x="-80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;350MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="24">
            <position x="-60.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="25">
            <position x="-40.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>AST Bravo 4/66d</object>
                    <object>ATronics ATI-486-66</object>
                    <object>Blue Star 466D2U</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>FCS 486-66</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                    <object>Mega Impact 486DX2/66E+</object>
                    <object>NCR System 3350</object>
                    <object>Naga Windows Workstation</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                    <object>Poly 486-66LM</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>Swan 486DX2-66DB</object>
                    <object>Tangent Model 466ex</object>
                    <object>Tri-Star 66/DX2-VL</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="26">
            <position x="-100.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;250MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="27">
            <position x="-80.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Super Computer 486X2/e66</object>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>BOSS 466d</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>Dell 466DE/2</object>
                    <object>Diamond DX2-66</object>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>Expo 486 dX2/66</object>
                    <object>Gecco 466E</object>
                    <object>HP Vectra 486/66U</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                    <object>Northgate SlimLine ZXP</object>
                    <object>Occidental 66MHz 486DX2</object>
                    <object>PCS Double Pro-66</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>USA Flex 486DX2/66</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="28">
            <position x="-120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="-3.0" y="-3.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="4.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>Arche Legacy 486/66DX2</object>
                    <object>Austin 466DX2 WinStation</object>
                    <object>BLK 486DX2/66</object>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>Broadax 486DX2-66</object>
                    <object>CAF Gold 6D2</object>
                    <object>Comex 486DX2/66</object>
                    <object>Compaq Deskpro 66M</object>
                    <object>CompuAdd 466E</object>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>DFI 486-66DX2</object>
                    <object>Edge 466 Magnum</object>
                    <object>Hyundai 466D2</object>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>NETiS Ultra WinStation N466L</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                    <object>Twinhead Superset 600/462D</object>
                    <object>Wyse Decision 486si</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&lt;225MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="29">
            <position x="0.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="1" to="8" />
        <edge from="2" to="3" />
        <edge from="2" to="9" />
        <edge from="3" to="4" />
        <edge from="3" to="10" />
        <edge from="4" to="5" />
        <edge from="4" to="11" />
        <edge from="5" to="6" />
        <edge from="5" to="12" />
        <edge from="6" to="7" />
        <edge from="6" to="13" />
        <edge from="7" to="29" />
        <edge from="8" to="9" />
        <edge from="8" to="14" />
        <edge from="9" to="10" />
        <edge from="9" to="15" />
        <edge from="10" to="11" />
        <edge from="10" to="16" />
        <edge from="11" to="12" />
        <edge from="11" to="17" />
        <edge from="12" to="13" />
        <edge from="12" to="18" />
        <edge from="13" to="29" />
        <edge from="14" to="15" />
        <edge from="14" to="19" />
        <edge from="15" to="16" />
        <edge from="15" to="20" />
        <edge from="16" to="17" />
        <edge from="16" to="21" />
        <edge from="17" to="18" />
        <edge from="17" to="22" />
        <edge from="18" to="29" />
        <edge from="19" to="20" />
        <edge from="19" to="23" />
        <edge from="20" to="21" />
        <edge from="20" to="24" />
        <edge from="21" to="22" />
        <edge from="21" to="25" />
        <edge from="22" to="29" />
        <edge from="23" to="24" />
        <edge from="23" to="26" />
        <edge from="24" to="25" />
        <edge from="24" to="27" />
        <edge from="25" to="29" />
        <edge from="26" to="27" />
        <edge from="26" to="28" />
        <edge from="27" to="29" />
        <edge from="28" to="29" />
    </diagram>
    <diagram title="Leistung des Netzteils und Zahl der Anschlüsse">
        <node id="1">
            <position x="0.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="2">
            <position x="0.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="9.0" y="-9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>5 Anschlüsse</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="0.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>Twinhead Superset 600/462D</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="4">
            <position x="0.0" y="180.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Austin 466DX2 WinStation</object>
                    <object>Broadax 486DX2-66</object>
                    <object>CAF Gold 6D2</object>
                    <object>CompuAdd 466E</object>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>Edge 466 Magnum</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>HP Vectra 486/66U</object>
                    <object>IDS 466i2</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                    <object>NETiS Ultra WinStation N466L</object>
                    <object>Osicom i466 MOD 420</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>Swan 486DX2-66DB</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                    <object>Wyse Decision 486si</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="5">
            <position x="0.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>BLK 486DX2/66</object>
                    <object>Comex 486DX2/66</object>
                    <object>DFI 486-66DX2</object>
                    <object>Gecco 466E</object>
                    <object>Mega Impact 486DX2/66E+</object>
                    <object>Poly 486-66LM</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="6">
            <position x="0.0" y="300.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Mitac TL4466</object>
                    <object>Expo 486 dX2/66</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="7">
            <position x="-180.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="-9.0" y="-9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>2 Anschlüsse</attribute>
                    <attribute>Leistung &lt;150 Watt</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="8">
            <position x="60.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="9.0" y="-9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>6 Anschlüsse</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="9">
            <position x="60.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="10">
            <position x="60.0" y="180.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Super Computer 486X2/e66</object>
                    <object>Hyundai 466D2</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="11">
            <position x="60.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>Lightning ThunderBox</object>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>NEC Express DX2/66e</object>
                    <object>USA Flex 486DX2/66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="12">
            <position x="60.0" y="300.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>BOSS 466d</object>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>FCS 486-66</object>
                    <object>FutureTech System 462E</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="13">
            <position x="120.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="9.0" y="-9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>7 Anschlüsse</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="14">
            <position x="120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="15">
            <position x="120.0" y="180.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Occidental 66MHz 486DX2</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="16">
            <position x="120.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="17">
            <position x="120.0" y="300.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="18">
            <position x="180.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="9.0" y="-9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>8 Anschlüsse</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="19">
            <position x="180.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="20">
            <position x="180.0" y="180.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="21">
            <position x="180.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="22">
            <position x="180.0" y="300.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Tri-Star 66/DX2-VL</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="23">
            <position x="-60.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="9.0" y="-9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>AST Bravo 4/66d</object>
                    <object>Digital DECpc 466d2 LP</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>4 Anschlüsse</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="24">
            <position x="-60.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>NCR System 3350</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="25">
            <position x="-60.0" y="180.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ATronics ATI-486-66</object>
                    <object>Arche Legacy 486/66DX2</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>Compaq Deskpro 66M</object>
                    <object>Dell 466DE/2</object>
                    <object>Diamond DX2-66</object>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>IBM PS/2 Model 77 486DX2</object>
                    <object>Insight 486DX2-66I</object>
                    <object>Naga Windows Workstation</object>
                    <object>PCS Double Pro-66</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>Tangent Model 466ex</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="26">
            <position x="-60.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Blue Star 466D2U</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="27">
            <position x="-60.0" y="300.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ariel 486DX2-66VLB</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="28">
            <position x="-120.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="9.0" y="-9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>3 Anschlüsse</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="29">
            <position x="-120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Northgate SlimLine ZXP</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="30">
            <position x="-120.0" y="180.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="31">
            <position x="-120.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="32">
            <position x="-120.0" y="300.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="33">
            <position x="-180.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="9.0" y="-9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;200 Watt</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="34">
            <position x="-180.0" y="180.0" />
            <attributeLabelStyle>
                <offset x="9.0" y="-9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>SST 486DX2-66MWC</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&lt;250 Watt</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="35">
            <position x="-180.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="9.0" y="-9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&lt;300 Watt</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="36">
            <position x="-180.0" y="300.0" />
            <attributeLabelStyle>
                <offset x="9.0" y="-9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;=300 Watt</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="37">
            <position x="0.0" y="360.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="1" to="7" />
        <edge from="1" to="8" />
        <edge from="1" to="13" />
        <edge from="1" to="18" />
        <edge from="1" to="23" />
        <edge from="1" to="28" />
        <edge from="2" to="3" />
        <edge from="3" to="4" />
        <edge from="4" to="5" />
        <edge from="5" to="6" />
        <edge from="6" to="37" />
        <edge from="7" to="33" />
        <edge from="8" to="9" />
        <edge from="9" to="10" />
        <edge from="10" to="11" />
        <edge from="11" to="12" />
        <edge from="12" to="37" />
        <edge from="13" to="14" />
        <edge from="14" to="15" />
        <edge from="15" to="16" />
        <edge from="16" to="17" />
        <edge from="17" to="37" />
        <edge from="18" to="19" />
        <edge from="19" to="20" />
        <edge from="20" to="21" />
        <edge from="21" to="22" />
        <edge from="22" to="37" />
        <edge from="23" to="24" />
        <edge from="24" to="25" />
        <edge from="25" to="26" />
        <edge from="26" to="27" />
        <edge from="27" to="37" />
        <edge from="28" to="29" />
        <edge from="29" to="30" />
        <edge from="30" to="31" />
        <edge from="31" to="32" />
        <edge from="32" to="37" />
        <edge from="33" to="34" />
        <edge from="34" to="35" />
        <edge from="35" to="36" />
        <edge from="36" to="37" />
    </diagram>
    <diagram title="Graphikkarte">
        <node id="1">
            <position x="-70.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="2">
            <position x="-280.0" y="105.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="10.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>ATronics ATI-486-66</object>
                    <object>Arche Legacy 486/66DX2</object>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>BLK 486DX2/66</object>
                    <object>BOSS 466d</object>
                    <object>Blue Star 466D2U</object>
                    <object>Broadax 486DX2-66</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>Comex 486DX2/66</object>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>Dell 466DE/2</object>
                    <object>Diamond DX2-66</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>Expo 486 dX2/66</object>
                    <object>FutureTech System 462E</object>
                    <object>IDS 466i2</object>
                    <object>Insight 486DX2-66I</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>Mega Impact 486DX2/66E+</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>Naga Windows Workstation</object>
                    <object>Osicom i466 MOD 420</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                    <object>PCS Double Pro-66</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>Tangent Model 466ex</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                    <object>USA Flex 486DX2/66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>ISA Bus</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="-280.0" y="175.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="10.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Mitac TL4466</object>
                    <object>American Super Computer 486X2/e66</object>
                    <object>Compaq Deskpro 66M</object>
                    <object>CompuAdd 466E</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>Gecco 466E</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>EISA Bus</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="4">
            <position x="-70.0" y="70.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="10.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Local Bus</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="5">
            <position x="70.0" y="140.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="10.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>AST Bravo 4/66d</object>
                    <object>CAF Gold 6D2</object>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>FCS 486-66</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>HP Vectra 486/66U</object>
                    <object>NCR System 3350</object>
                    <object>NEC Express DX2/66e</object>
                    <object>Northgate SlimLine ZXP</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                    <object>Twinhead Superset 600/462D</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Motherboard</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="6">
            <position x="-210.0" y="140.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="10.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>IBM PS/2 Model 77 486DX2</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>MCA</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="7">
            <position x="0.0" y="140.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="10.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ariel 486DX2-66VLB</object>
                    <object>Austin 466DX2 WinStation</object>
                    <object>Hyundai 466D2</object>
                    <object>Lightning ThunderBox</object>
                    <object>Occidental 66MHz 486DX2</object>
                    <object>Tri-Star 66/DX2-VL</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>VESA Local Bus</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="8">
            <position x="-140.0" y="140.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="10.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>Edge 466 Magnum</object>
                    <object>NETiS Ultra WinStation N466L</object>
                    <object>Poly 486-66LM</object>
                    <object>Swan 486DX2-66DB</object>
                    <object>Wyse Decision 486si</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Proprietary Local Bus</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="9">
            <position x="-70.0" y="140.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="10.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>DFI 486-66DX2</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>UBSA Local BUS</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="10">
            <position x="-70.0" y="245.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="1" to="4" />
        <edge from="1" to="5" />
        <edge from="1" to="6" />
        <edge from="2" to="3" />
        <edge from="3" to="10" />
        <edge from="4" to="7" />
        <edge from="4" to="8" />
        <edge from="4" to="9" />
        <edge from="5" to="10" />
        <edge from="6" to="10" />
        <edge from="7" to="10" />
        <edge from="8" to="10" />
        <edge from="9" to="10" />
    </diagram>
    <diagram title="Zugängliche Laufwerke">
        <node id="1">
            <position x="0.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="-17.5" y="-12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>kein 3½" Schacht</attribute>
                    <attribute>kein 5¼" Schacht</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="2">
            <position x="50.0" y="50.0" />
            <attributeLabelStyle>
                <offset x="15.0" y="-15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>ein 3½" Schacht</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="100.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="15.0" y="-15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>zwei 3½" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="4">
            <position x="150.0" y="150.0" />
            <attributeLabelStyle>
                <offset x="15.0" y="-15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>drei 3½" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="5">
            <position x="-50.0" y="50.0" />
            <attributeLabelStyle>
                <offset x="-4.5" y="-14.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>ein 5¼" Schacht</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="6">
            <position x="0.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>NCR System 3350</object>
                    <object>NETiS Ultra WinStation N466L</object>
                    <object>Northgate SlimLine ZXP</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="7">
            <position x="50.0" y="150.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="8">
            <position x="100.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="9">
            <position x="-100.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="-5.5" y="-15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>AST Bravo 4/66d</object>
                    <object>Arche Legacy 486/66DX2</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>zwei 5¼" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="10">
            <position x="-50.0" y="150.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>Blue Star 466D2U</object>
                    <object>Comex 486DX2/66</object>
                    <object>CompuAdd 466E</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>HP Vectra 486/66U</object>
                    <object>NEC Express DX2/66e</object>
                    <object>Twinhead Superset 600/462D</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="11">
            <position x="0.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>BLK 486DX2/66</object>
                    <object>Lightning ThunderBox</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="12">
            <position x="50.0" y="250.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Wyse Decision 486si</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="13">
            <position x="-150.0" y="150.0" />
            <attributeLabelStyle>
                <offset x="-11.0" y="-14.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Austin 466DX2 WinStation</object>
                    <object>Broadax 486DX2-66</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>Compaq Deskpro 66M</object>
                    <object>Dell 466DE/2</object>
                    <object>Edge 466 Magnum</object>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>FCS 486-66</object>
                    <object>IDS 466i2</object>
                    <object>Memorex Telex 8092-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>drei 5¼" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="14">
            <position x="-100.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>IBM PS/2 Model 77 486DX2</object>
                    <object>Insight 486DX2-66I</object>
                    <object>Osicom i466 MOD 420</object>
                    <object>PCS Double Pro-66</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>Standard Windows Workstation Plus</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="15">
            <position x="-50.0" y="250.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ATronics ATI-486-66</object>
                    <object>BOSS 466d</object>
                    <object>CAF Gold 6D2</object>
                    <object>Diamond DX2-66</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>Hyundai 466D2</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                    <object>Naga Windows Workstation</object>
                    <object>Poly 486-66LM</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>Tangent Model 466ex</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="16">
            <position x="0.0" y="300.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="17">
            <position x="-200.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="-8.0" y="-16.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>DFI 486-66DX2</object>
                    <object>Gecco 466E</object>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                    <object>Mega Impact 486DX2/66E+</object>
                    <object>Occidental 66MHz 486DX2</object>
                    <object>Tri-Star 66/DX2-VL</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>vier 5¼" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="18">
            <position x="-150.0" y="250.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Super Computer 486X2/e66</object>
                    <object>C² Saber 486/e DX2-66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="19">
            <position x="-100.0" y="300.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="20">
            <position x="-50.0" y="350.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="21">
            <position x="-250.0" y="250.0" />
            <attributeLabelStyle>
                <offset x="-7.5" y="-12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ariel 486DX2-66VLB</object>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                    <object>Swan 486DX2-66DB</object>
                    <object>USA Flex 486DX2/66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>fünf 5¼" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="22">
            <position x="-200.0" y="300.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="23">
            <position x="-150.0" y="350.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Keydata 486DX2-66 KeyStation</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="24">
            <position x="-100.0" y="400.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="25">
            <position x="-300.0" y="300.0" />
            <attributeLabelStyle>
                <offset x="-7.0" y="-15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Expo 486 dX2/66</object>
                    <object>FutureTech System 462E</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>sechs 5¼" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="26">
            <position x="-250.0" y="350.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="27">
            <position x="-200.0" y="400.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="28">
            <position x="-150.0" y="450.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="29">
            <position x="-350.0" y="350.0" />
            <attributeLabelStyle>
                <offset x="-10.0" y="-14.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Mitac TL4466</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>sieben 5¼" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="30">
            <position x="-300.0" y="400.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="31">
            <position x="-250.0" y="450.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="32">
            <position x="-200.0" y="500.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="1" to="5" />
        <edge from="2" to="3" />
        <edge from="2" to="6" />
        <edge from="3" to="4" />
        <edge from="3" to="7" />
        <edge from="4" to="8" />
        <edge from="5" to="6" />
        <edge from="5" to="9" />
        <edge from="6" to="7" />
        <edge from="6" to="10" />
        <edge from="7" to="8" />
        <edge from="7" to="11" />
        <edge from="8" to="12" />
        <edge from="9" to="10" />
        <edge from="9" to="13" />
        <edge from="10" to="11" />
        <edge from="10" to="14" />
        <edge from="11" to="12" />
        <edge from="11" to="15" />
        <edge from="12" to="16" />
        <edge from="13" to="14" />
        <edge from="13" to="17" />
        <edge from="14" to="15" />
        <edge from="14" to="18" />
        <edge from="15" to="16" />
        <edge from="15" to="19" />
        <edge from="16" to="20" />
        <edge from="17" to="18" />
        <edge from="17" to="21" />
        <edge from="18" to="19" />
        <edge from="18" to="22" />
        <edge from="19" to="20" />
        <edge from="19" to="23" />
        <edge from="20" to="24" />
        <edge from="21" to="22" />
        <edge from="21" to="25" />
        <edge from="22" to="23" />
        <edge from="22" to="26" />
        <edge from="23" to="24" />
        <edge from="23" to="27" />
        <edge from="24" to="28" />
        <edge from="25" to="26" />
        <edge from="25" to="29" />
        <edge from="26" to="27" />
        <edge from="26" to="30" />
        <edge from="27" to="28" />
        <edge from="27" to="31" />
        <edge from="28" to="32" />
        <edge from="29" to="30" />
        <edge from="30" to="31" />
        <edge from="31" to="32" />
    </diagram>
    <diagram title="Bustypen der 486/66 PCs">
        <node id="1">
            <position x="0.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="2">
            <position x="80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="12.0" y="-12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="12.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>AST Bravo 4/66d</object>
                    <object>ATronics ATI-486-66</object>
                    <object>Arche Legacy 486/66DX2</object>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>Ariel 486DX2-66VLB</object>
                    <object>Austin 466DX2 WinStation</object>
                    <object>BLK 486DX2/66</object>
                    <object>BOSS 466d</object>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>Blue Star 466D2U</object>
                    <object>Broadax 486DX2-66</object>
                    <object>CAF Gold 6D2</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>Comex 486DX2/66</object>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>DFI 486-66DX2</object>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>Expo 486 dX2/66</object>
                    <object>FCS 486-66</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>Hyundai 466D2</object>
                    <object>IDS 466i2</object>
                    <object>Insight 486DX2-66I</object>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                    <object>Lightning ThunderBox</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                    <object>NETiS Ultra WinStation N466L</object>
                    <object>Naga Windows Workstation</object>
                    <object>Northgate SlimLine ZXP</object>
                    <object>Occidental 66MHz 486DX2</object>
                    <object>Osicom i466 MOD 420</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                    <object>PCS Double Pro-66</object>
                    <object>Poly 486-66LM</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                    <object>Swan 486DX2-66DB</object>
                    <object>Tri-Star 66/DX2-VL</object>
                    <object>Twinhead Superset 600/462D</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                    <object>Wyse Decision 486si</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>ISA-Bus</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="80.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="12.0" y="-12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="12.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Mitac TL4466</object>
                    <object>American Super Computer 486X2/e66</object>
                    <object>Compaq Deskpro 66M</object>
                    <object>CompuAdd 466E</object>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>Dell 466DE/2</object>
                    <object>Diamond DX2-66</object>
                    <object>Edge 466 Magnum</object>
                    <object>FutureTech System 462E</object>
                    <object>Gecco 466E</object>
                    <object>HP Vectra 486/66U</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>Mega Impact 486DX2/66E+</object>
                    <object>NEC Express DX2/66e</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>Tangent Model 466ex</object>
                    <object>USA Flex 486DX2/66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>EISA-Bus</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="4">
            <position x="-80.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="-12.0" y="-12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="-12.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>IBM PS/2 Model 77 486DX2</object>
                    <object>NCR System 3350</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>MCA-Bus</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="5">
            <position x="0.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="1" to="4" />
        <edge from="2" to="3" />
        <edge from="3" to="5" />
        <edge from="4" to="5" />
    </diagram>
    <diagram title="Gehäusetyp">
        <node id="1">
            <position x="0.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="2">
            <position x="50.0" y="50.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="7.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>Austin 466DX2 WinStation</object>
                    <object>Blue Star 466D2U</object>
                    <object>Broadax 486DX2-66</object>
                    <object>CAF Gold 6D2</object>
                    <object>Comex 486DX2/66</object>
                    <object>Compaq Deskpro 66M</object>
                    <object>CompuAdd 466E</object>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>HP Vectra 486/66U</object>
                    <object>IBM PS/2 Model 77 486DX2</object>
                    <object>Insight 486DX2-66I</object>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                    <object>Lightning ThunderBox</object>
                    <object>NEC Express DX2/66e</object>
                    <object>NETiS Ultra WinStation N466L</object>
                    <object>Naga Windows Workstation</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                    <object>PCS Double Pro-66</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>Swan 486DX2-66DB</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Desktop</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="100.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="7.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>AST Bravo 4/66d</object>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>Edge 466 Magnum</object>
                    <object>Northgate SlimLine ZXP</object>
                    <object>Wyse Decision 486si</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Slimline</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="4">
            <position x="0.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="7.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ATronics ATI-486-66</object>
                    <object>Arche Legacy 486/66DX2</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>Dell 466DE/2</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>Hyundai 466D2</object>
                    <object>IDS 466i2</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>NCR System 3350</object>
                    <object>Osicom i466 MOD 420</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>Twinhead Superset 600/462D</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Small-footprint</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="5">
            <position x="-50.0" y="50.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="7.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Mitac TL4466</object>
                    <object>American Super Computer 486X2/e66</object>
                    <object>Ariel 486DX2-66VLB</object>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>DFI 486-66DX2</object>
                    <object>Diamond DX2-66</object>
                    <object>Expo 486 dX2/66</object>
                    <object>FCS 486-66</object>
                    <object>FutureTech System 462E</object>
                    <object>Gecco 466E</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>Mega Impact 486DX2/66E+</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                    <object>Occidental 66MHz 486DX2</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                    <object>Poly 486-66LM</object>
                    <object>Tangent Model 466ex</object>
                    <object>Tri-Star 66/DX2-VL</object>
                    <object>USA Flex 486DX2/66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Tower</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="6">
            <position x="-100.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-8.75" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="7.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>BLK 486DX2/66</object>
                    <object>BOSS 466d</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Mini-Tower</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="7">
            <position x="0.0" y="175.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="1" to="5" />
        <edge from="2" to="3" />
        <edge from="2" to="4" />
        <edge from="3" to="7" />
        <edge from="4" to="7" />
        <edge from="5" to="6" />
        <edge from="6" to="7" />
    </diagram>
    <diagram title="WinMarks (Graphics/Disk) für 486/66 PCs">
        <node id="1">
            <position x="0.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>Disk WinMark &gt; 0, Graphics WinMark &gt; 0</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="2">
            <position x="40.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt; 5</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt; 10</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="4">
            <position x="120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt; 15</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="5">
            <position x="160.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt; 20</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="6">
            <position x="200.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt; 30</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="7">
            <position x="240.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt; 40</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="8">
            <position x="-40.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="-20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&gt; 15</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="9">
            <position x="-80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="-20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>AST Bravo 4/66d</object>
                    <object>BLK 486DX2/66</object>
                    <object>Broadax 486DX2-66</object>
                    <object>CAF Gold 6D2</object>
                    <object>Gecco 466E</object>
                    <object>Insight 486DX2-66I</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>Twinhead Superset 600/462D</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&gt; 30</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="10">
            <position x="-120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="-20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                    <object>NCR System 3350</object>
                    <object>NEC Express DX2/66e</object>
                    <object>Naga Windows Workstation</object>
                    <object>PCS Double Pro-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&gt; 45</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="11">
            <position x="-160.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="-20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt; 60</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="12">
            <position x="-200.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="-20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt; 90</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="13">
            <position x="-240.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="-20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt; 120</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="14">
            <position x="0.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ATronics ATI-486-66</object>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>DFI 486-66DX2</object>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                    <object>Tangent Model 466ex</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="15">
            <position x="40.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="16">
            <position x="80.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="17">
            <position x="120.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="18">
            <position x="160.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="19">
            <position x="200.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="20">
            <position x="-40.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>American Super Computer 486X2/e66</object>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>Blue Star 466D2U</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>Comex 486DX2/66</object>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>FCS 486-66</object>
                    <object>HP Vectra 486/66U</object>
                    <object>IBM PS/2 Model 77 486DX2</object>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>Mega Impact 486DX2/66E+</object>
                    <object>NETiS Ultra WinStation N466L</object>
                    <object>Occidental 66MHz 486DX2</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>USA Flex 486DX2/66</object>
                    <object>Wyse Decision 486si</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="21">
            <position x="-80.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Mitac TL4466</object>
                    <object>CompuAdd 466E</object>
                    <object>Dell 466DE/2</object>
                    <object>Diamond DX2-66</object>
                    <object>Expo 486 dX2/66</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>IDS 466i2</object>
                    <object>Northgate SlimLine ZXP</object>
                    <object>Osicom i466 MOD 420</object>
                    <object>Poly 486-66LM</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="22">
            <position x="-120.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>FutureTech System 462E</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="23">
            <position x="-160.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="24">
            <position x="-200.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="25">
            <position x="0.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Compaq Deskpro 66M</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                    <object>Swan 486DX2-66DB</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="26">
            <position x="40.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>Lightning ThunderBox</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="27">
            <position x="80.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Hyundai 466D2</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="28">
            <position x="120.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="29">
            <position x="160.0" y="320.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Arche Legacy 486/66DX2</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="30">
            <position x="-40.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="31">
            <position x="-80.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="32">
            <position x="-120.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="33">
            <position x="-160.0" y="320.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Edge 466 Magnum</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="34">
            <position x="0.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ariel 486DX2-66VLB</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="35">
            <position x="40.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Austin 466DX2 WinStation</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>Tri-Star 66/DX2-VL</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="36">
            <position x="80.0" y="320.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="37">
            <position x="120.0" y="360.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>BOSS 466d</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="38">
            <position x="-40.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="39">
            <position x="-80.0" y="320.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="40">
            <position x="-120.0" y="360.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="41">
            <position x="0.0" y="320.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="42">
            <position x="40.0" y="360.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="43">
            <position x="80.0" y="400.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="44">
            <position x="-40.0" y="360.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="45">
            <position x="-80.0" y="400.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="46">
            <position x="0.0" y="400.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="47">
            <position x="40.0" y="440.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="48">
            <position x="-40.0" y="440.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="49">
            <position x="0.0" y="480.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="1" to="8" />
        <edge from="2" to="3" />
        <edge from="2" to="14" />
        <edge from="3" to="4" />
        <edge from="3" to="15" />
        <edge from="4" to="5" />
        <edge from="4" to="16" />
        <edge from="5" to="6" />
        <edge from="5" to="17" />
        <edge from="6" to="7" />
        <edge from="6" to="18" />
        <edge from="7" to="19" />
        <edge from="8" to="9" />
        <edge from="8" to="14" />
        <edge from="9" to="10" />
        <edge from="9" to="20" />
        <edge from="10" to="11" />
        <edge from="10" to="21" />
        <edge from="11" to="12" />
        <edge from="11" to="22" />
        <edge from="12" to="13" />
        <edge from="12" to="23" />
        <edge from="13" to="24" />
        <edge from="14" to="15" />
        <edge from="14" to="20" />
        <edge from="15" to="16" />
        <edge from="15" to="25" />
        <edge from="16" to="17" />
        <edge from="16" to="26" />
        <edge from="17" to="18" />
        <edge from="17" to="27" />
        <edge from="18" to="19" />
        <edge from="18" to="28" />
        <edge from="19" to="29" />
        <edge from="20" to="21" />
        <edge from="20" to="25" />
        <edge from="21" to="22" />
        <edge from="21" to="30" />
        <edge from="22" to="23" />
        <edge from="22" to="31" />
        <edge from="23" to="24" />
        <edge from="23" to="32" />
        <edge from="24" to="33" />
        <edge from="25" to="26" />
        <edge from="25" to="30" />
        <edge from="26" to="27" />
        <edge from="26" to="34" />
        <edge from="27" to="28" />
        <edge from="27" to="35" />
        <edge from="28" to="29" />
        <edge from="28" to="36" />
        <edge from="29" to="37" />
        <edge from="30" to="31" />
        <edge from="30" to="34" />
        <edge from="31" to="32" />
        <edge from="31" to="38" />
        <edge from="32" to="33" />
        <edge from="32" to="39" />
        <edge from="33" to="40" />
        <edge from="34" to="35" />
        <edge from="34" to="38" />
        <edge from="35" to="36" />
        <edge from="35" to="41" />
        <edge from="36" to="37" />
        <edge from="36" to="42" />
        <edge from="37" to="43" />
        <edge from="38" to="39" />
        <edge from="38" to="41" />
        <edge from="39" to="40" />
        <edge from="39" to="44" />
        <edge from="40" to="45" />
        <edge from="41" to="42" />
        <edge from="41" to="44" />
        <edge from="42" to="43" />
        <edge from="42" to="46" />
        <edge from="43" to="47" />
        <edge from="44" to="45" />
        <edge from="44" to="46" />
        <edge from="45" to="48" />
        <edge from="46" to="47" />
        <edge from="46" to="48" />
        <edge from="47" to="49" />
        <edge from="48" to="49" />
    </diagram>
    <diagram title="Vertriebsform">
        <node id="1">
            <position x="0.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="2">
            <position x="-70.0" y="70.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-17.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>Direktvertrieb</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="-140.0" y="140.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-17.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="21.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ariel 486DX2-66VLB</object>
                    <object>BLK 486DX2/66</object>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>Blue Star 466D2U</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>Diamond DX2-66</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>Edge 466 Magnum</object>
                    <object>Expo 486 dX2/66</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>Gecco 466E</object>
                    <object>IDS 466i2</object>
                    <object>Insight 486DX2-66I</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                    <object>Naga Windows Workstation</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                    <object>Northgate SlimLine ZXP</object>
                    <object>Occidental 66MHz 486DX2</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                    <object>PCS Double Pro-66</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>Swan 486DX2-66DB</object>
                    <object>Tangent Model 466ex</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                    <object>USA Flex 486DX2/66</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Nur Direktvertrieb</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="4">
            <position x="70.0" y="70.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-17.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>Händler</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="5">
            <position x="140.0" y="140.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-17.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="21.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>AST Bravo 4/66d</object>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>BOSS 466d</object>
                    <object>CAF Gold 6D2</object>
                    <object>Comex 486DX2/66</object>
                    <object>Compaq Deskpro 66M</object>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>DFI 486-66DX2</object>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>HP Vectra 486/66U</object>
                    <object>IBM PS/2 Model 77 486DX2</object>
                    <object>NEC Express DX2/66e</object>
                    <object>Wyse Decision 486si</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Nur Händlervertrieb</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="6">
            <position x="0.0" y="140.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-17.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="21.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>ATronics ATI-486-66</object>
                    <object>American Mitac TL4466</object>
                    <object>American Super Computer 486X2/e66</object>
                    <object>Arche Legacy 486/66DX2</object>
                    <object>Austin 466DX2 WinStation</object>
                    <object>Broadax 486DX2-66</object>
                    <object>CompuAdd 466E</object>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>Dell 466DE/2</object>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>FCS 486-66</object>
                    <object>FutureTech System 462E</object>
                    <object>Hyundai 466D2</object>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                    <object>Lightning ThunderBox</object>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>Mega Impact 486DX2/66E+</object>
                    <object>NCR System 3350</object>
                    <object>NETiS Ultra WinStation N466L</object>
                    <object>Osicom i466 MOD 420</object>
                    <object>Poly 486-66LM</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                    <object>Tri-Star 66/DX2-VL</object>
                    <object>Twinhead Superset 600/462D</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Beide Vertriebsformen</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="7">
            <position x="0.0" y="210.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="1" to="4" />
        <edge from="2" to="3" />
        <edge from="2" to="6" />
        <edge from="3" to="7" />
        <edge from="4" to="5" />
        <edge from="4" to="6" />
        <edge from="5" to="7" />
        <edge from="6" to="7" />
    </diagram>
    <diagram title="Festplattengrößen (ordinal)">
        <node id="1">
            <position x="0.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="15.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="2">
            <position x="0.0" y="100.0" />
            <attributeLabelStyle>
                <offset x="15.0" y="-15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="15.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>AST Bravo 4/66d</object>
                    <object>ATronics ATI-486-66</object>
                    <object>American Mitac TL4466</object>
                    <object>American Super Computer 486X2/e66</object>
                    <object>Arche Legacy 486/66DX2</object>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>Austin 466DX2 WinStation</object>
                    <object>BLK 486DX2/66</object>
                    <object>BOSS 466d</object>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>Blue Star 466D2U</object>
                    <object>Broadax 486DX2-66</object>
                    <object>CAF Gold 6D2</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>Comex 486DX2/66</object>
                    <object>Compaq Deskpro 66M</object>
                    <object>CompuAdd 466E</object>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>DFI 486-66DX2</object>
                    <object>Dell 466DE/2</object>
                    <object>Diamond DX2-66</object>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>Edge 466 Magnum</object>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>Expo 486 dX2/66</object>
                    <object>FCS 486-66</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>Gecco 466E</object>
                    <object>HP Vectra 486/66U</object>
                    <object>Hyundai 466D2</object>
                    <object>IDS 466i2</object>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                    <object>Lightning ThunderBox</object>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>Mega Impact 486DX2/66E+</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                    <object>NCR System 3350</object>
                    <object>NETiS Ultra WinStation N466L</object>
                    <object>Naga Windows Workstation</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                    <object>Northgate SlimLine ZXP</object>
                    <object>Occidental 66MHz 486DX2</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                    <object>PCS Double Pro-66</object>
                    <object>Poly 486-66LM</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>Swan 486DX2-66DB</object>
                    <object>Tangent Model 466ex</object>
                    <object>Tri-Star 66/DX2-VL</object>
                    <object>Twinhead Superset 600/462D</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                    <object>USA Flex 486DX2/66</object>
                    <object>Wyse Decision 486si</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&gt;=200MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="0.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="15.0" y="-15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="15.0" y="15.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ariel 486DX2-66VLB</object>
                    <object>FutureTech System 462E</object>
                    <object>IBM PS/2 Model 77 486DX2</object>
                    <object>Insight 486DX2-66I</object>
                    <object>NEC Express DX2/66e</object>
                    <object>Osicom i466 MOD 420</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&gt;=400MB</attribute>
                </attributeContingent>
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="2" to="3" />
    </diagram>
    <diagram title="DOS-Win-Mark">
        <node id="1">
            <position x="0.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>Disk WinMark &gt; 0, DOSmark &gt;= 0</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="2">
            <position x="40.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;= 40</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;= 50</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="4">
            <position x="120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;= 60</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="5">
            <position x="160.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;= 70</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="6">
            <position x="200.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;= 80</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="7">
            <position x="240.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt;= 90</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="8">
            <position x="-40.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="-20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&gt; 15</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="9">
            <position x="-80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="-20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>AST Bravo 4/66d</object>
                    <object>American Super Computer 486X2/e66</object>
                    <object>Arche Legacy 486/66DX2</object>
                    <object>BLK 486DX2/66</object>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>Blue Star 466D2U</object>
                    <object>Broadax 486DX2-66</object>
                    <object>CAF Gold 6D2</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>Comex 486DX2/66</object>
                    <object>Compaq Deskpro 66M</object>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>FCS 486-66</object>
                    <object>Gecco 466E</object>
                    <object>HP Vectra 486/66U</object>
                    <object>Hyundai 466D2</object>
                    <object>Lightning ThunderBox</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>Occidental 66MHz 486DX2</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>Swan 486DX2-66DB</object>
                    <object>Twinhead Superset 600/462D</object>
                    <object>USA Flex 486DX2/66</object>
                    <object>Wyse Decision 486si</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&gt; 30</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="10">
            <position x="-120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="-20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>Ariel 486DX2-66VLB</object>
                    <object>Austin 466DX2 WinStation</object>
                    <object>CompuAdd 466E</object>
                    <object>Diamond DX2-66</object>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                    <object>NCR System 3350</object>
                    <object>NEC Express DX2/66e</object>
                    <object>Naga Windows Workstation</object>
                    <object>Northgate SlimLine ZXP</object>
                    <object>Osicom i466 MOD 420</object>
                    <object>Tri-Star 66/DX2-VL</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>&gt; 45</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="11">
            <position x="-160.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="-20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt; 60</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="12">
            <position x="-200.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="-20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt; 90</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="13">
            <position x="-240.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="-20.0" y="-20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent>
                    <attribute>&gt; 120</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="14">
            <position x="0.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="15">
            <position x="40.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ATronics ATI-486-66</object>
                    <object>DFI 486-66DX2</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="16">
            <position x="80.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="17">
            <position x="120.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Tangent Model 466ex</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="18">
            <position x="160.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="19">
            <position x="200.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="20">
            <position x="-40.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>Insight 486DX2-66I</object>
                    <object>Mega Impact 486DX2/66E+</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="21">
            <position x="-80.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>BOSS 466d</object>
                    <object>Dell 466DE/2</object>
                    <object>Expo 486 dX2/66</object>
                    <object>IDS 466i2</object>
                    <object>PCS Double Pro-66</object>
                    <object>Poly 486-66LM</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="22">
            <position x="-120.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="23">
            <position x="-160.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="24">
            <position x="-200.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="25">
            <position x="0.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>NETiS Ultra WinStation N466L</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="26">
            <position x="40.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>IBM PS/2 Model 77 486DX2</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="27">
            <position x="80.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="28">
            <position x="120.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="29">
            <position x="160.0" y="320.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="30">
            <position x="-40.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>National Microsystems Flash 486DX2-66E</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="31">
            <position x="-80.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="32">
            <position x="-120.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="33">
            <position x="-160.0" y="320.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="34">
            <position x="0.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Mitac TL4466</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="35">
            <position x="40.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="36">
            <position x="80.0" y="320.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="37">
            <position x="120.0" y="360.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="38">
            <position x="-40.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>FutureTech System 462E</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="39">
            <position x="-80.0" y="320.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="40">
            <position x="-120.0" y="360.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="41">
            <position x="0.0" y="320.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="42">
            <position x="40.0" y="360.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="43">
            <position x="80.0" y="400.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="44">
            <position x="-40.0" y="360.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="45">
            <position x="-80.0" y="400.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="46">
            <position x="0.0" y="400.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="47">
            <position x="40.0" y="440.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="48">
            <position x="-40.0" y="440.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="49">
            <position x="0.0" y="480.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="20.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Edge 466 Magnum</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="1" to="8" />
        <edge from="2" to="3" />
        <edge from="2" to="14" />
        <edge from="3" to="4" />
        <edge from="3" to="15" />
        <edge from="4" to="5" />
        <edge from="4" to="16" />
        <edge from="5" to="6" />
        <edge from="5" to="17" />
        <edge from="6" to="7" />
        <edge from="6" to="18" />
        <edge from="7" to="19" />
        <edge from="8" to="9" />
        <edge from="8" to="14" />
        <edge from="9" to="10" />
        <edge from="9" to="20" />
        <edge from="10" to="11" />
        <edge from="10" to="21" />
        <edge from="11" to="12" />
        <edge from="11" to="22" />
        <edge from="12" to="13" />
        <edge from="12" to="23" />
        <edge from="13" to="24" />
        <edge from="14" to="15" />
        <edge from="14" to="20" />
        <edge from="15" to="16" />
        <edge from="15" to="25" />
        <edge from="16" to="17" />
        <edge from="16" to="26" />
        <edge from="17" to="18" />
        <edge from="17" to="27" />
        <edge from="18" to="19" />
        <edge from="18" to="28" />
        <edge from="19" to="29" />
        <edge from="20" to="21" />
        <edge from="20" to="25" />
        <edge from="21" to="22" />
        <edge from="21" to="30" />
        <edge from="22" to="23" />
        <edge from="22" to="31" />
        <edge from="23" to="24" />
        <edge from="23" to="32" />
        <edge from="24" to="33" />
        <edge from="25" to="26" />
        <edge from="25" to="30" />
        <edge from="26" to="27" />
        <edge from="26" to="34" />
        <edge from="27" to="28" />
        <edge from="27" to="35" />
        <edge from="28" to="29" />
        <edge from="28" to="36" />
        <edge from="29" to="37" />
        <edge from="30" to="31" />
        <edge from="30" to="34" />
        <edge from="31" to="32" />
        <edge from="31" to="38" />
        <edge from="32" to="33" />
        <edge from="32" to="39" />
        <edge from="33" to="40" />
        <edge from="34" to="35" />
        <edge from="34" to="38" />
        <edge from="35" to="36" />
        <edge from="35" to="41" />
        <edge from="36" to="37" />
        <edge from="36" to="42" />
        <edge from="37" to="43" />
        <edge from="38" to="39" />
        <edge from="38" to="41" />
        <edge from="39" to="40" />
        <edge from="39" to="44" />
        <edge from="40" to="45" />
        <edge from="41" to="42" />
        <edge from="41" to="44" />
        <edge from="42" to="43" />
        <edge from="42" to="46" />
        <edge from="43" to="47" />
        <edge from="44" to="45" />
        <edge from="44" to="46" />
        <edge from="45" to="48" />
        <edge from="46" to="47" />
        <edge from="46" to="48" />
        <edge from="47" to="49" />
        <edge from="48" to="49" />
    </diagram>
    <diagram title="Ports auf dem Motherboard">
        <node id="1">
            <position x="-30.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ATronics ATI-486-66</object>
                    <object>American Mitac TL4466</object>
                    <object>American Super Computer 486X2/e66</object>
                    <object>Arche Legacy 486/66DX2</object>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>Blue Star 466D2U</object>
                    <object>Broadax 486DX2-66</object>
                    <object>CAF Gold 6D2</object>
                    <object>Clover 486 Quick-I Series</object>
                    <object>Comex 486DX2/66</object>
                    <object>CompuAdd 466E</object>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>DFI 486-66DX2</object>
                    <object>Diamond DX2-66</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>Edge 466 Magnum</object>
                    <object>Expo 486 dX2/66</object>
                    <object>FCS 486-66</object>
                    <object>FutureTech System 462E</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>Gecco 466E</object>
                    <object>Insight 486DX2-66I</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>Mega Impact 486DX2/66E+</object>
                    <object>NETiS Ultra WinStation N466L</object>
                    <object>Naga Windows Workstation</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                    <object>Osicom i466 MOD 420</object>
                    <object>Poly 486-66LM</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>Tangent Model 466ex</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                    <object>USA Flex 486DX2/66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="2">
            <position x="-30.0" y="60.0" />
            <attributeLabelStyle>
                <offset x="-6.300000000000001" y="-9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                    <object>PCS Double Pro-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Ein paraller Port</attribute>
                    <attribute>Ein serieller Port</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="-30.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-10.5" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>NCR System 3350</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>ein Mouse-Port</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="4">
            <position x="30.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="4.800000000000001" y="-8.100000000000001" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>BLK 486DX2/66</object>
                    <object>BOSS 466d</object>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>IDS 466i2</object>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                    <object>Lightning ThunderBox</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>NEC Express DX2/66e</object>
                    <object>Northgate SlimLine ZXP</object>
                    <object>Occidental 66MHz 486DX2</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                    <object>PC Pros 486/66DX2 5550T</object>
                    <object>Swan 486DX2-66DB</object>
                    <object>Tri-Star 66/DX2-VL</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Zwei serielle Ports</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="5">
            <position x="30.0" y="180.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Compaq Deskpro 66M</object>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>HP Vectra 486/66U</object>
                    <object>Hyundai 466D2</object>
                    <object>IBM PS/2 Model 77 486DX2</object>
                    <object>Twinhead Superset 600/462D</object>
                    <object>Wyse Decision 486si</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="6">
            <position x="-90.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="-3.5999999999999996" y="-9.600000000000001" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>Ariel 486DX2-66VLB</object>
                    <object>Austin 466DX2 WinStation</object>
                    <object>Gateway 2000 4DX2-66V</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>Zwei parallele Ports</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="7">
            <position x="-90.0" y="180.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="9.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>AST Bravo 4/66d</object>
                    <object>Dell 466DE/2</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="8">
            <position x="-30.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="2" to="3" />
        <edge from="2" to="4" />
        <edge from="2" to="6" />
        <edge from="3" to="5" />
        <edge from="3" to="7" />
        <edge from="4" to="5" />
        <edge from="5" to="8" />
        <edge from="6" to="7" />
        <edge from="7" to="8" />
    </diagram>
    <diagram title="interne Laufwerksschächte">
        <node id="1">
            <position x="0.0" y="-0.0" />
            <attributeLabelStyle>
                <offset x="-11.6" y="-13.6" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ATronics ATI-486-66</object>
                    <object>American Mitac TL4466</object>
                    <object>HP Vectra 486/66U</object>
                    <object>IBM PS/2 Model 77 486DX2</object>
                    <object>Naga Windows Workstation</object>
                    <object>PC Brand Leader Cache 486/DX2-66</object>
                    <object>PCS Double Pro-66</object>
                    <object>Quill Qtech 486 4D2/66</object>
                    <object>SST 486DX2-66MWC</object>
                    <object>U.S. Micro Jet 486DX2-66</object>
                    <object>Wyse Decision 486si</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>kein 3½" Schacht</attribute>
                    <attribute>kein 5¼" Schacht</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="2">
            <position x="-40.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="-14.0" y="-12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>ALR Flyer 32DT 4DX2/66</object>
                    <object>Arche Legacy 486/66DX2</object>
                    <object>BOSS 466d</object>
                    <object>Micro Express ME 486-Local Bus/DX2/66</object>
                    <object>Northgate SlimLine ZXP</object>
                    <object>QSI Klonimus 486DX2/66</object>
                    <object>USA Flex 486DX2/66</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>ein 5½" Schacht</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="3">
            <position x="-80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="-10.4" y="-14.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>AST Bravo 4/66d</object>
                    <object>Ares 486-66DX2 VL-Bus</object>
                    <object>Blue Star 466D2U</object>
                    <object>DFI 486-66DX2</object>
                    <object>EPS ISA 486 DX2/66</object>
                    <object>Everex Tempo M Series 486 DX2/66</object>
                    <object>Expo 486 dX2/66</object>
                    <object>FCS 486-66</object>
                    <object>Gateway 2000 4DX2-66V</object>
                    <object>Int. Instr. Blue Max Monolith 486D2/66UP</object>
                    <object>NEC Express DX2/66e</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>zwei 5½" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="4">
            <position x="-120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="-11.2" y="-13.6" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>FutureTech System 462E</object>
                    <object>ZDS Z-Station 466Xh Model 200</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>drei 5½" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="5">
            <position x="40.0" y="40.0" />
            <attributeLabelStyle>
                <offset x="12.0" y="-12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>BLK 486DX2/66</object>
                    <object>Compaq Deskpro 66M</object>
                    <object>CompuAdd Express 466DX Scalable</object>
                    <object>Dell 466DE/2</object>
                    <object>GCH EasyData 486DX-2/66</object>
                    <object>Insight 486DX2-66I</object>
                    <object>Keydata 486DX2-66 KeyStation</object>
                    <object>Memorex Telex 8092-66</object>
                    <object>NETiS Ultra WinStation N466L</object>
                    <object>Osicom i466 MOD 420</object>
                    <object>Poly 486-66LM</object>
                    <object>Silicon Pylon II 486DXi-212</object>
                    <object>Standard Windows Workstation Plus</object>
                    <object>Swan 486DX2-66DB</object>
                    <object>Tangent Model 466ex</object>
                    <object>Twinhead Superset 600/462D</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>ein 3¼" Schacht</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="6">
            <position x="0.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Comex 486DX2/66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="7">
            <position x="-40.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Bi-Link Desktop i486DX2/66</object>
                    <object>Zeos 486DX2-66</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="8">
            <position x="-80.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="9">
            <position x="80.0" y="80.0" />
            <attributeLabelStyle>
                <offset x="12.0" y="-12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Austin 466DX2 WinStation</object>
                    <object>Broadax 486DX2-66</object>
                    <object>CAF Gold 6D2</object>
                    <object>Comtrade 486 EISA Dream Machine</object>
                    <object>Diamond DX2-66</object>
                    <object>Digital DECpc 466d2 LP</object>
                    <object>Edge 466 Magnum</object>
                    <object>Hyundai 466D2</object>
                    <object>IDS 466i2</object>
                    <object>Lightning ThunderBox</object>
                    <object>NCR System 3350</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>zwei 3¼" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="10">
            <position x="40.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="11">
            <position x="0.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Ariel 486DX2-66VLB</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="12">
            <position x="-40.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="13">
            <position x="120.0" y="120.0" />
            <attributeLabelStyle>
                <offset x="12.0" y="-12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>PC Pros 486/66DX2 5550T</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>drei 3¼" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="14">
            <position x="80.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Gecco 466E</object>
                    <object>Mega Impact 486DX2/66E+</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="15">
            <position x="40.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>LodeStar 486-DX2/66 EISA WINstation</object>
                    <object>National Microsystems Flash 486DX2-66E</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="16">
            <position x="0.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="17">
            <position x="-160.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="-12.0" y="-13.2" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>right</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>American Super Computer 486X2/e66</object>
                    <object>C² Saber 486/e DX2-66</object>
                    <object>Tri-Star 66/DX2-VL</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>vier 5½" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="18">
            <position x="-120.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="19">
            <position x="-80.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="20">
            <position x="-40.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="21">
            <position x="160.0" y="160.0" />
            <attributeLabelStyle>
                <offset x="12.0" y="-12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Clover 486 Quick-I Series</object>
                    <object>CompuAdd 466E</object>
                </objectContingent>
                <attributeContingent>
                    <attribute>vier 3¼" Schächte</attribute>
                </attributeContingent>
            </concept>
        </node>
        <node id="22">
            <position x="120.0" y="200.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="23">
            <position x="80.0" y="240.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="12.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>center</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent>
                    <object>Occidental 66MHz 486DX2</object>
                </objectContingent>
                <attributeContingent />
            </concept>
        </node>
        <node id="24">
            <position x="40.0" y="280.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <node id="25">
            <position x="0.0" y="320.0" />
            <attributeLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </attributeLabelStyle>
            <objectLabelStyle>
                <offset x="0.0" y="-0.0" />
                <backgroundColor>#ffffffff</backgroundColor>
                <textColor>#ff000000</textColor>
                <textAlignment>left</textAlignment>
            </objectLabelStyle>
            <concept>
                <objectContingent />
                <attributeContingent />
            </concept>
        </node>
        <edge from="1" to="2" />
        <edge from="1" to="5" />
        <edge from="2" to="3" />
        <edge from="2" to="6" />
        <edge from="3" to="4" />
        <edge from="3" to="7" />
        <edge from="4" to="8" />
        <edge from="4" to="17" />
        <edge from="5" to="6" />
        <edge from="5" to="9" />
        <edge from="6" to="7" />
        <edge from="6" to="10" />
        <edge from="7" to="8" />
        <edge from="7" to="11" />
        <edge from="8" to="12" />
        <edge from="8" to="18" />
        <edge from="9" to="10" />
        <edge from="9" to="13" />
        <edge from="10" to="11" />
        <edge from="10" to="14" />
        <edge from="11" to="12" />
        <edge from="11" to="15" />
        <edge from="12" to="16" />
        <edge from="12" to="19" />
        <edge from="13" to="14" />
        <edge from="13" to="21" />
        <edge from="14" to="15" />
        <edge from="14" to="22" />
        <edge from="15" to="16" />
        <edge from="15" to="23" />
        <edge from="16" to="20" />
        <edge from="16" to="24" />
        <edge from="17" to="18" />
        <edge from="18" to="19" />
        <edge from="19" to="20" />
        <edge from="20" to="25" />
        <edge from="21" to="22" />
        <edge from="22" to="23" />
        <edge from="23" to="24" />
        <edge from="24" to="25" />
    </diagram>
</conceptualSchema>

