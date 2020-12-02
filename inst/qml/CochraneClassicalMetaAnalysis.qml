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
import "./common"		as MM

Form {
	id: form

	TextArea
	{
		text:		"Acute Respiratory Infections; Airways; Anaesthesia; Back and Neck; Bone, Joint and Muscle Trauma; Common Mental Disorders; Cystic Fibrosis and Genetic Disorders; Dementia and Cognitive Improvement; Effective Practice and Organisation of Care; Gynaecology and Fertility; Hepato-Biliary; Infectious Diseases; Inflammatory Bowel Disease; Injuries; Movement Disorders; Musculoskeletal; Neonatal; Pregnancy and Childbirth; Schizophrenia; Stroke; Vascular"
		visible:	false
		height:		100
		name:		"availableTopics"
		textType:	"source"
		separators:	[";"]
	}

	TextArea
	{
		text:		"Acupuncture Therapy; Acute Disease; Adaptation, Physiological; Administration, Inhalation; Administration, Oral; Adolescent; Adrenal Cortex Hormones; Adrenergic beta 2 Receptor Agonists; Adrenergic beta Agonists; Adult; Aftercare; Age Factors; Aged; Aged, 80 and over; Alzheimer Disease; Ambulatory Care; Amnion; Analgesia, Obstetrical; Anesthesia, Conduction; Anesthesia, General; Animals; Anti Anxiety Agents; Anti Asthmatic Agents; Anti Bacterial Agents; Anti Inflammatory Agents; Anti Inflammatory Agents, Non Steroidal; Antibiotic Prophylaxis; Antifibrinolytic Agents; Antimalarials; Antiparkinson Agents; Antipsychotic Agents; Antirheumatic Agents; Aphasia; Apnea; Arachidonic Acid; Arginine; Artemisinins; Arteriosclerosis; Arthritis, Psoriatic; Arthritis, Rheumatoid; Asthma; Athletic Injuries; Atropine; Auranofin; Azathioprine; Balneology; Bandages; Barbiturates; Bed Rest; Benzodiazepines; beta Lactams; Binge Eating Disorder; Birth Injuries; Blood Transfusion; Body Temperature Regulation; Body Weight; Brain Injuries; Brain Ischemia; Breech Presentation; Bromocriptine; Bronchitis; Bronchodilator Agents; Bronchopulmonary Dysplasia; Budesonide; Bulimia Nervosa; Caffeine; Calcium Channel Blockers; Cardiovascular Agents; Central Nervous System Agents; Central Nervous System Stimulants; Cephalometry; Cerebral Hemorrhage; Cerebral Ventricles; Cerebrovascular Circulation; Cerebrovascular Disorders; Cesarean Section; Child; Child Development; Child Nutrition Disorders; Child, Preschool; Chlorpromazine; Cholinergic Agents; Cholinergic Antagonists; Chronic Disease; Clonazepam; Clozapine; Cognition Disorders; Cognitive Behavioral Therapy; Common Cold; Community Pharmacy Services; Congenital Abnormalities; Constriction, Pathologic; Continuity of Patient Care; Contraceptive Agents, Female; Contraceptives, Oral, Combined; Controlled Clinical Trials as Topic; Cost Benefit Analysis; Cough; Crohn Disease; Cross Infection; Cyclooxygenase 2 Inhibitors; Cyclosporine; Cystic Fibrosis; Cytidine Diphosphate Choline; Danazol; Deglutition; Deglutition Disorders; Dehydration; Delivery of Health Care; Delivery, Obstetric; Dementia; Dementia, Vascular; Dermatologic Agents; Developed Countries; Developing Countries; Developmental Disabilities; Dietary Proteins; Dietary Supplements; Disease Progression; Docosahexaenoic Acids; Dopamine Agents; Dose Response Relationship, Drug; Double Blind Method; Drinking Water; Drug Administration Schedule; Drug Substitution; Drug Therapy, Combination; Ductus Arteriosus, Patent; Dyskinesia, Drug Induced; Early Ambulation; Echinacea; Elective Surgical Procedures; Electric Stimulation Therapy; Emergency Service, Hospital; Endometrial Ablation Techniques; Endometriosis; Endometritis; Endometrium; Endoscopy; Energy Intake; Enteral Nutrition; Enterocolitis, Necrotizing; Enzyme Inhibitors; Equipment Design; Ergoloid Mesylates; Esophageal and Gastric Varices; Estrogen Antagonists; Etretinate; Evoked Potentials, Visual; Exercise Therapy; Extracorporeal Membrane Oxygenation; Family Practice; Fatty Acids, Unsaturated; Female; Fetal Distress; Food, Fortified; Fracture Fixation; Fractures, Stress; Fumarates; G(M1) Ganglioside; GABA Modulators; Gangliosides; Gastrointestinal Hemorrhage; Gastrostomy; Growth; Headache; Health Care Costs; Heart Rate, Fetal; Heating; Hemostatics; Hepatic Encephalopathy; Hernia, Diaphragmatic; High Frequency Ventilation; Hip Fractures; Home Care Services, Hospital Based; Home Nursing; Hospital Units; Hospitalization; Humans; Hydrotherapy; Hypertension; Hypolipidemic Agents; Hypotension; Hysterectomy; Hysteroscopy; Immersion; Immobilization; Immunoglobulins, Intravenous; Immunologic Factors; Immunosuppressive Agents; Incubators, Infant; Indomethacin; Induction Chemotherapy; Infant; Infant Care; Infant Formula; Infant Mortality; Infant Nutritional Physiological Phenomena; Infant, Extremely Low Birth Weight; Infant, Low Birth Weight; Infant, Newborn; Infant, Newborn, Diseases; Infant, Premature; Infant, Premature, Diseases; Infant, Small for Gestational Age; Infant, Very Low Birth Weight; Infections; Inhalation Spacers; Injections; Injections, Intravenous; Intelligence; Intensive Care Units, Neonatal; Intention to Treat Analysis; Intracranial Hemorrhages; Intracranial Hypertension; Intracranial Pressure; Intrauterine Devices, Medicated; Intubation, Gastrointestinal; Ipratropium; Labor Stage, First; Labor Stage, Second; Language Therapy; Leg; Leg Injuries; Length of Stay; Levodopa; Levonorgestrel; Lisinopril; Liver Cirrhosis; Low Back Pain; Macrolides; Magnesium; Malaria; Malaria, Falciparum; Male; Massage; Maternal Child Nursing; Meconium; Mefenamic Acid; Memory; Menorrhagia; Mental Disorders; Mental Health Services; Mesalamine; Methotrexate; Metoclopramide; Middle Aged; Midwifery; Military Personnel; Milk; Milk, Human; Monoamine Oxidase Inhibitors; Mortality; Mosquito Control; Mud Therapy; Naproxen; Natural Childbirth; Nausea; Nebulizers and Vaporizers; Nerve Block; Neuroprotective Agents; Nifedipine; Nimodipine; Nitric Oxide; Nitric Oxide Donors; Nitric Oxide Synthase; Nitroglycerin; Nootropic Agents; Obstetric Labor, Premature; Octreotide; Office Visits; Oligohydramnios; Organogold Compounds; Orthotic Devices; Osteoarthritis; Outcome Assessment, Health Care; Pain Management; Parenteral Nutrition; Parkinson Disease; Patient Acceptance of Health Care; Patient Care; Patient Care Team; Patient Discharge; Patient Dropouts; Patient Education as Topic; Patient Readmission; Patient Satisfaction; Pelvic Pain; Pergolide; Perinatal Mortality; Perineum; Peripheral Vascular Diseases; Pharmacists; Phenobarbital; Physical Stimulation; Physical Therapy Modalities; Phytotherapy; Placebo Effect; Placebos; Plant Extracts; Pneumonia; Portasystemic Shunt, Surgical; Portasystemic Shunt, Transjugular Intrahepatic; Postnatal Care; Postoperative Complications; Practice Patterns, Physicians'; Pre Eclampsia; Pregnancy; Pregnancy Complications, Infectious; Pregnancy Complications, Parasitic; Pregnancy Outcome; Pregnancy, High Risk; Pregnancy, Multiple; Premature Birth; Prenatal Care; Prescription Drugs; Primary Health Care; Professional Practice; Professional Role; Progestins; Prognosis; Program Evaluation; Psychotherapy; Psychotic Disorders; Psychotropic Drugs; Publication Bias; Quality of Life; Radon; Randomized Controlled Trials as Topic; Referral and Consultation; Remission Induction; Respiratory Insufficiency; Risk; Risperidone; Schizophrenia; Secondary Prevention; Selegiline; Self Care; Sepsis; Sesquiterpenes; Shoes; Shoulder Fractures; Social Support; Somatostatin; Speech Therapy; Splenorenal Shunt, Surgical; Stillbirth; Stroke; Stroke Rehabilitation; Substance Related Disorders; Sulfasalazine; Term Birth; Theophylline; Thioridazine; Time Factors; Transcranial Direct Current Stimulation; Treatment Failure; Treatment Outcome; Twins; Umbilical Cord; Uterus; Vasodilator Agents; Vision, Ocular; Visual Acuity; Vitamin D; Vitamin E; Vitamin K; Vitamins; Water; Weight Gain; Weight Loss; Withholding Treatment; Zinc"
		visible:	false
		height:		100
		name:		"availableKeywords"
		textType:	"source"
		separators:	[";"]
	}

	RadioButtonGroup
	{
		columns:	3
		name:		"selectionType"
		title:		qsTr("Select meta-analyses based on")
		id:			dataType

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
			source:	"availableTopics"
		}

		AssignedVariablesList
		{
			name:	"topicsSelected"
			title:	qsTr("Selected")
		}
	}

	VariablesForm
	{
		preferredHeight:	300
		visible:			selectionKeywords.checked

		AvailableVariablesList
		{
			name:	"keywordsMenu"
			title:	qsTr("Keywords")
			source:	"availableKeywords"
		}

		AssignedVariablesList
		{
			name:	"keywordsSelected"
			title:	qsTr("Selected")
		}
	}

	TextArea
	{
		Layout.columnSpan:	2
		height:		300
		visible:	selectionTextSearch.checked
		text:		""
		name:		"textSearch"
		textType:	"source"
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

	Section
	{
		title: qsTr("Model")

		DropDown
		{
			Layout.columnSpan:	2
			name: "method"; label: qsTr("Method"); currentIndex: 2
			values: [
				{ label: qsTr("Fixed Effects")	, value: "Fixed Effects"		},
				{ label: qsTr("Likelihood")		, value: "Likelihood"			},
				{ label: qsTr("Restricted ML")	, value: "Restricted ML"		},
				{ label: "DerSimonian-Laird"	, value: "DerSimonian-Laird"	},
				{ label: qsTr("Hedges")			, value: "Hedges"				},
				{ label: "Hunter-Schmidt"		, value: "Hunter-Schmidt"		},
				{ label: "Sidik-Jonkman"		, value: "Sidik-Jonkman"		},
				{ label: qsTr("Empirical Bayes"), value: "Empirical Bayes"		},
				{ label: "Paule-Mandel"			, value: "Paule-Mandel"			}
			]
		}


		Group
		{
			title: qsTr("Regression Coefficients")
            CheckBox
            {   name: "regressionCoefficientsEstimates";
                text: qsTr("Estimates");
                checked: true
                onClicked: { if (!checked && estimatesConfInt.checked) estimatesConfInt.click() }
                CheckBox
                {
                    id: estimatesConfInt
                    name: "regressionCoefficientsConfidenceIntervals"; text: qsTr("Confidence intervals")
                    CIField { name: "regressionCoefficientsConfidenceIntervalsInterval"; label: qsTr("Interval") }
                    DropDown { name: "test"; label: qsTr("Test"); values: [ "z", "knha"]; }
                }
            }
			CheckBox { name: "regressionCoefficientsCovarianceMatrix"; text: qsTr("Covariance matrix") }

		}
		Group
		{
			title: qsTr("Model Fit")
			CheckBox { name: "modelFit";				text: qsTr("Fit measures") }
			CheckBox { name: "forestPlot";				text: qsTr("Forest plot") }
			CheckBox { name: "funnelPlot";				text: qsTr("Funnel plot") }
			CheckBox { name: "rSquaredChange";			text: qsTr("Rank test for funnel plot asymmetry") }
			CheckBox { name: "funnelPlotAsymmetryTest"; text: qsTr("Regression test for funnel plot asymmetry") }
		}

		Group
		{
			title: qsTr("Residuals Model")
			CheckBox { name: "residualsParameters"; text: qsTr("Residuals parameters"); checked: true;}
		}
	}

	Section
	{
		title: qsTr("Diagnostics")
		Group
		{
			title: qsTr("Plots")
			CheckBox { name: "trimFillPlot";			text: qsTr("Trim-fill analysis")	}
			CheckBox { name: "plotResidualsPredicted";	text: qsTr("Profile plot")			}
			CheckBox
			{
				name: "plotResidualsDependent"; text: qsTr("Diagnostic plots")
				CheckBox { name: "plotResidualsQQ"; text: qsTr("Q-Q plot standardized residuals"); checked: true }
			}
		}
		Group
		{
			title: qsTr("Robustness")
			CheckBox { name: "plotResidualsCovariates";			text: qsTr("Fail-safe N")			}
			CheckBox { name: "residualsCasewiseDiagnostics";	text: qsTr("Casewise diagnostics")	}
		}
	}
}
