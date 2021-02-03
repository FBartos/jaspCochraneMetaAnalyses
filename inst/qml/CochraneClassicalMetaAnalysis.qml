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
import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import QtQuick.Layouts	1.3
import "../qml/qml_components" as MA  // TODO: link to the meta-analysis repository 

Form {
	id: form

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

		MA.ClassicalMetaAnalysisMethod{}
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
			Label { text: qsTr("Study");				Layout.preferredWidth: 250 * preferencesModel.uiScale; Layout.leftMargin: 5 * preferencesModel.uiScale}
			Label { text: qsTr("Effect Size");			Layout.preferredWidth: 100 * preferencesModel.uiScale }
			Label { text: qsTr("Standard Error");		Layout.preferredWidth: 100 * preferencesModel.uiScale }
			Label { text: qsTr("Confidence Interval") }
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
					Layout.preferredWidth:	250 * preferencesModel.uiScale
					TextField
					{
						name: 				"titleStudy"
						useExternalBorder: 	true
					}
				}

				Row
				{
					spacing:				4 * preferencesModel.uiScale
					Layout.preferredWidth: 100 * preferencesModel.uiScale
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
					spacing:				4 * preferencesModel.uiScale
					Layout.preferredWidth: 100 * preferencesModel.uiScale
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
					Layout.preferredWidth: 155 * preferencesModel.uiScale
					TextField
					{
						name:				"lCI"
						id:					lCI
						value:				""
						fieldWidth: 		40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value == ""
					}
					TextField
					{
						name:				"uCI"
						id:					uCI
						value:				""
						fieldWidth: 		40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value == ""
					}
				}
			}
		}
	}


	MA.ClassicalMetaAnalysisStatistics{}

	MA.ClassicalMetaAnalysisDiagnostics{}
}
