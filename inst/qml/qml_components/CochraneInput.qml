//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import "../qml_components" as MA

Section
{
	title: 		qsTr("Database")
	expanded:	true

	property string analysisType:	"classicalContinuous"

	RadioButtonGroup
	{
		columns:			4
		name:				"analyzeAs"
		title:				qsTr("Analyze as")
		Layout.columnSpan:	2
		visible:			analysisType == "classicalDichotomous" || analysisType == "bayesianDichotomous"

		RadioButton
		{
			value:		"OR"
			label:		qsTr("Odds ratios")
			checked:	true
		}

		RadioButton
		{
			value:		"POR"
			label:		qsTr("Peto's odds ratios")
		}

		RadioButton
		{
			value:		"RR"
			label:		qsTr("Risk ratios")
		}

		RadioButton
		{
			value:		"RD"
			label:		qsTr("Risk differences")
		}
	}

	RadioButtonGroup
	{
		columns:			3
		name:				"selectionType"
		title:				qsTr("Select meta-analyses based on")
		id:					dataType
		Layout.columnSpan:	2

		RadioButton
		{
			value:		"selectionTopics"
			label:		qsTr("Topics")
			id:			selectionTopics
			checked:	true
		}

		RadioButton
		{
			value:		"selectionKeywords"
			label:		qsTr("Keywords")
			id:			selectionKeywords
		}

		RadioButton
		{
			value:		"selectionTextSearch"
			label:		qsTr("Search titles")
			id:			selectionTextSearch
		}
	}

	VariablesForm
	{
		preferredHeight:	300
		visible:			selectionTopics.checked

		AvailableVariablesList
		{
			name:	"topicsMenu"
			title:	qsTr("Topics")
			source:	[{ rSource: "sourceTopics"}]
		}

		AssignedVariablesList
		{
			name:	"topicsSelected"
			title:	qsTr("Selected")
		}
	}


	Group
	{
		visible:			selectionKeywords.checked
		Layout.columnSpan:	2

		VariablesForm
		{
			preferredHeight:	300
			

			AvailableVariablesList
			{
				height:	250
				name:	"keywordsMenu"
				title:	qsTr("Keywords")
				source:	[{ rSource: "sourceKeywords"}]
			}

			AssignedVariablesList
			{
				name:	"keywordsSelected"
				title:	qsTr("Selected")
			}
		}

		TextField
		{
			name:	"keywordsSearch"
			label:	qsTr("Search")
			value:	""
		}
	}

	TextArea
	{
		Layout.columnSpan:	2
		height:		300
		visible:	selectionTextSearch.checked
		text:		""
		name:		"textSearch"
		textType:	JASP.TextTypeSource
	}


	ComponentsList
	{
		Layout.columnSpan:		2
		name: 					"selectionGadget"
		source:					[{ rSource: "selectionGadget"}]
		preferredHeight:		250

		rowComponent: CheckBox { name: "selected"; label: rowValue; checked: false}
	}


	Group
	{

		RadioButtonGroup
		{
			columns:	1
			name:		"analyzeData"
			title:		qsTr("Analyze Data")

			RadioButton
			{
				value:		"reviews"
				label:		qsTr("Reviews")
				checked:	true
			}

			RadioButton
			{
				value:		"metaAnalyses"
				label:		qsTr("Meta-analyses")
			}

			RadioButton
			{
				value:		"together"
				label:		qsTr("Together")
			}
		}

		MA.ClassicalMetaAnalysisMethod
		{
			visible:	analysisType == "classicalContinuous" || analysisType == "classicalDichotomous"
		}
	}

	Group
	{
		title: 	qsTr("Plot")
		CheckBox
		{
			name:		"plotEffectSizes"
			label:		qsTr("Effect sizes")
		}

		CheckBox
		{
			name:		"plotSampleSizes"
			label:		qsTr("Sample sizes")
		}

		CheckBox
		{
			name:		"distPlotDensity"
			label:		qsTr("Display density")
		}

		CheckBox
		{
			name:		"distPlotRug"
			label:		qsTr("Display rug marks")
		}

		DropDown
		{
			name:		"binWidthType"
			label:		qsTr("Bin Width Type")
			indexDefaultValue: 0
			values:
			[
				{label: qsTr("Sturges"),				value: "sturges"},
				{label: qsTr("Scott"),					value: "scott"},
				{label: qsTr("Doane"),					value: "doane"},
				{label: qsTr("Freedman-Diaconis"),		value: "fd"	},
				{label: qsTr("Manual"),					value: "manual"}
			]
			id: binWidthType
		}

		DoubleField
		{
			name:			"numberOfBins"
			label:			qsTr("Number of Bins")
			defaultValue:	30
			min:			3;
			max:			10000;
			enabled:		binWidthType.currentValue === "manual"
		}
	}

	CheckBox
	{
		Layout.columnSpan:	2
		name:				"addStudy"
		label:				qsTr("Add estimates")
		id:					addStudy
	}

	ColumnLayout
	{
		spacing:				0
		visible:				addStudy.checked
		Layout.preferredWidth:	parent.width
		Layout.columnSpan:		2

		RowLayout
		{
			Label
			{
				text:					qsTr("Study")
				Layout.preferredWidth:	200 * preferencesModel.uiScale; Layout.leftMargin: 5 * preferencesModel.uiScale
			}
			Label
			{
				text:					qsTr("Effect Size")
				Layout.preferredWidth:  50 * preferencesModel.uiScale
			}
			Label
			{
				text:					qsTr("Standard Error")
				Layout.preferredWidth:	if (analysisType == "classicalContinuous" || analysisType == "bayesianContinuous") {100 * preferencesModel.uiScale} else {75 * preferencesModel.uiScale}
			}
			Label
			{
				text:					if (analysisType == "classicalContinuous" || analysisType == "bayesianContinuous") {qsTr("Confidence Interval")} else {qsTr("Frequencies")}
			}
		}

		ComponentsList
		{
			name:					"additionalStudies"
			optionKey:				"name"
			Layout.maximumHeight:	200
			rowComponent: 			RowLayout
			{
				Row
				{
					spacing:				4 * preferencesModel.uiScale
					Layout.preferredWidth:	200 * preferencesModel.uiScale
					TextField
					{
						name: 				"titleStudy"
						useExternalBorder: 	true
					}
				}

				Row
				{
					spacing:				4 * preferencesModel.uiScale
					Layout.preferredWidth: 50 * preferencesModel.uiScale
					TextField
					{
						name:				"effectSize"
						value:				""
						fieldWidth: 		40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
					}
				}

				Row
				{
					spacing:				 4 * preferencesModel.uiScale
					Layout.preferredWidth:	if (analysisType == "classicalContinuous" || analysisType == "bayesianContinuous") {100 * preferencesModel.uiScale} else {75 * preferencesModel.uiScale}
					TextField
					{
						name:				"effectSE"
						id:					effectSE
						value:				""
						fieldWidth: 		40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			lCI.value == "" && uCI.value == ""
					}
				}

				Row
				{
					spacing:				4 * preferencesModel.uiScale
					Layout.preferredWidth:	if (analysisType == "classicalContinuous" || analysisType == "bayesianContinuous") {155 * preferencesModel.uiScale} else {150 * preferencesModel.uiScale}
					TextField
					{
						name:				"lCI"
						label:				qsTr("lCI")
						id:					lCI
						visible:			analysisType == "classicalContinuous" || analysisType == "bayesianContinuous"
						value:				""
						fieldWidth: 		40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value == ""
					}
					TextField
					{
						name:				"uCI"
						label:				qsTr("uCI")
						visible:			analysisType == "classicalContinuous" || analysisType == "bayesianContinuous"
						id:					uCI
						value:				""
						fieldWidth: 		40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value == ""
					}
					TextField
					{
						name:				"x1"
						label:				qsTr("x₁")
						id:					x1
						visible:			analysisType == "classicalDichotomous" || analysisType == "bayesianDichotomous"
						value:				""
						fieldWidth: 		30 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value == ""
					}
					TextField
					{
						name:				"n1"
						label:				qsTr("n₁")
						id:					n1
						visible:			analysisType == "classicalDichotomous" || analysisType == "bayesianDichotomous"
						value:				""
						fieldWidth: 		30 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value == ""
					}
					TextField
					{
						name:				"x2"
						label:				qsTr("x₂")
						visible:			analysisType == "classicalDichotomous" || analysisType == "bayesianDichotomous"
						id:					x2
						value:				""
						fieldWidth: 		30 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value == ""
					}
					TextField
					{
						name:				"n2"
						label:				qsTr("n₂")
						visible:			analysisType == "classicalDichotomous" || analysisType == "bayesianDichotomous"
						id:					n2
						value:				""
						fieldWidth: 		30 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value == ""
					}
				}
			}
		}
	}
}