import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "jaspCochraneMetaAnalyses"
	title		: qsTr("Cochrane Meta-Analyses")
	description	: qsTr("This module allows to analyze Cochrane medical datasets.")
	icon		: "cochraneLogo.svg"
	version		: "0.14"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	requiresData: false
		

	Analysis
	{
		title:	qsTr("Classical Meta-Analysis")
		func:	"CochraneClassicalMetaAnalysis"
	}

}
