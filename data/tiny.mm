<map version="freeplane 1.3.0">
<!--To view this file, download free mind mapping software Freeplane from http://freeplane.sourceforge.net -->
<node TEXT="the root" ID="ID_1723255651" CREATED="1283093380553" MODIFIED="1449261674074"><hook NAME="MapStyle" zoom="2.357">

<map_styles>
<stylenode LOCALIZED_TEXT="styles.root_node">
<stylenode LOCALIZED_TEXT="styles.predefined" POSITION="right">
<stylenode LOCALIZED_TEXT="default" MAX_WIDTH="600" COLOR="#000000" STYLE="as_parent">
<font NAME="SansSerif" SIZE="10" BOLD="false" ITALIC="false"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.details"/>
<stylenode LOCALIZED_TEXT="defaultstyle.note"/>
<stylenode LOCALIZED_TEXT="defaultstyle.floating">
<edge STYLE="hide_edge"/>
<cloud COLOR="#f0f0f0" SHAPE="ROUND_RECT"/>
</stylenode>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.user-defined" POSITION="right">
<stylenode LOCALIZED_TEXT="styles.topic" COLOR="#18898b" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subtopic" COLOR="#cc3300" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subsubtopic" COLOR="#669900">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.important">
<icon BUILTIN="yes"/>
</stylenode>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.AutomaticLayout" POSITION="right">
<stylenode LOCALIZED_TEXT="AutomaticLayout.level.root" COLOR="#000000">
<font SIZE="18"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,1" COLOR="#0033ff">
<font SIZE="16"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,2" COLOR="#00b439">
<font SIZE="14"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,3" COLOR="#990000">
<font SIZE="12"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,4" COLOR="#111111">
<font SIZE="10"/>
</stylenode>
</stylenode>
</stylenode>
</map_styles>
</hook>
<hook NAME="AutomaticEdgeColor" COUNTER="4"/>

<node TEXT="1, linked to 2 and 2.1" POSITION="right" ID="ID_1588678987" CREATED="1449192253308" MODIFIED="1449261657470">
<edge COLOR="#ff0000"/>
<arrowlink SHAPE="CUBIC_CURVE" COLOR="#000000" WIDTH="2" TRANSPARENCY="80" FONT_SIZE="9" FONT_FAMILY="SansSerif" DESTINATION="ID_1481921831" STARTINCLINATION="40;0;" ENDINCLINATION="40;0;" STARTARROW="NONE" ENDARROW="DEFAULT"/>
<arrowlink SHAPE="CUBIC_CURVE" COLOR="#000000" WIDTH="2" TRANSPARENCY="80" FONT_SIZE="9" FONT_FAMILY="SansSerif" DESTINATION="ID_1441026734" STARTINCLINATION="123;0;" ENDINCLINATION="123;0;" STARTARROW="NONE" ENDARROW="DEFAULT"/>
<node TEXT="1.1" ID="ID_281013951" CREATED="1449261543591" MODIFIED="1449261625920"/>
<node TEXT="1.2" ID="ID_385709394" CREATED="1449261633118" MODIFIED="1449261635545">
<node TEXT="1.2.1" ID="ID_1961068114" CREATED="1449261637582" MODIFIED="1449261640313"/>
<node TEXT="1.2.2" ID="ID_1904562482" CREATED="1449261637582" MODIFIED="1449261644449"/>
</node>
</node>

<node TEXT="2" POSITION="right" ID="ID_1481921831" CREATED="1449192255923" MODIFIED="1449261659966">
<edge COLOR="#0000ff"/>
<node TEXT="2.1" ID="ID_1441026734" CREATED="1449192255923" MODIFIED="1449261628551"/>
</node>

<node TEXT="fat node, with a newline right here:&#xa;these are the escape characters: &lt;&gt;&quot;&amp;&apos;&#xa;caret is not: ^" POSITION="right" ID="ID_1716485631" CREATED="1449193573345" MODIFIED="1449216947328">
<edge COLOR="#ff00ff"/>
</node>
</node>
</map>
