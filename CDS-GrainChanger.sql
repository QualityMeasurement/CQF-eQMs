
/*  
QUERY NAME:	  ClinicalDecisionMetricAlerts
DESCRIPTION:  This binding provides encounter grain and alert grain (distinguishable by the EncounterGrainFLG)
             information on alerts and the actions taken.

CHANGE LOG:
__________________________________________________________________________________________________________________________
DATE (CYYY-JJJ)  |DATA ARCHITECT     |CHANGE DESCRIPTION
___________________________________________________________________________________________________
julian x.12 date    David Balkcom       Added HAR (hospital account record) unique identifier to 
                                        all 3 result sets; (EPIC.Finance.HospitalAccount) 
                                        PatientEncounterID was previously relied on as unique 
                                        identifier, however, a single inpatient stay can have 
                                        multiple instances of PatientEncounterID because each 
                                        service / resource creates a new encounterID, and therefore
                                        is not unique at the appropriate grain (appropriate grain: 
                                        the patient themselves)
*/
WITH LastRace AS (
	-- Get the most recent documented race for each patient
	SELECT
    		rb.PatientID
    		, MAX(rb.LineNBR) AS MaxLineNBR
	FROM Epic.Patient.Race rb 
	GROUP BY rb.PatientID 	 
)
, pre
    AS (SELECT
		   Alerts.AlertCSNID
		 , Alerts.AlertID
		 , Alerts.BPAFLG
		 , Alerts.MedicationFLG
		 , CASE 

		   /**For Medication alerts**/

		   WHEN Alerts.MedicationFLG = 1
			  THEN NULL

		   /**For BPAs**/
		   WHEN Alerts.BPAFLG = 1
			  THEN CASE WHEN BPATypes.BPATypeDSC IS NOT NULL
					 THEN BPATypes.BPATypeDSC
				  ELSE 'Unknown'
				  END
		   END AS BPATypeDSC
		 , CASE 

		   /**For Medication alerts**/

		   WHEN Alerts.MedicationFLG = 1
			  THEN Alerts.MedicationAlertTypeDSC 

		   /**For BPAs**/
		   WHEN Alerts.BPAFLG = 1
			  THEN 'BPA'
		   END AS TypeDSC
		 , Alerts.AlertDSC
		 , Alerts.ActionTakenCD
        , Alerts.ActionTakenDSC
		 , Alerts.MedicationAlertTypeCD
		 , Alerts.MedicationAlertTypeDSC
		 , Alerts.BestPracticeTriggerActionCD
		 , /**Not applicable for Medication information**/
		  /**Not applicable for Medication information**/
          	   Alerts.BackgroundFireFLG
		 , Alerts.SpecificOverrideReasonCD
		 , Alerts.SpecificOverrideReasonDSC
		 , Alerts.SpecificOverrideCommentTXT
		 , Alerts.PatientEncounterID
         , hab.HospitalAccountID                --new UID
		 , Alerts.ContactDTS
		 , Alerts.PatientDepartmentID
		 , Alerts.PatientID
		 , Alerts.UserID
		 , Alerts.EmployeeNM
		 , Alerts.ProviderID
		 , Alerts.ProviderNM
		 , Alerts.ProviderTypeCD
		 , Alerts.ProviderTypeNM
		 , psb.ReportGrouper13CD AS SpecialtyCD
		 , psb.ReportGrouper13DSC AS SpecialtyDSC
		 , CASE WHEN hab.HospitalAccountClassCD IS NULL
			  THEN CASE WHEN peb.EncounterTypeCD = 101 -- Office Visit
					 THEN 'Outpatient'
				  ELSE NULL
				  END
		   ELSE hab.HospitalAccountClassDSC
		   END AS HospitalClassDSC
		 , hab.LocationID
		 , lb.RevenueLocationNM
		 , ib.DiagnosisNM
		 , ib2.DiagnosisNM AS ParentDiagnosisNM
		 , CASE WHEN cb.CoverageTypeCD = 2 AND vcb.BenefitPlanID IN (70145, 70147, 70148)
					THEN 1
				WHEN cb.CoverageTypeCD != 2 AND cb.PlanID IN (70145, 70147, 70148)
					THEN 1
						ELSE 0
						END AS SHCAPatient
		 , fcb.FinancialClassCD
        , fcb.FinancialClassNM
		 , CASE 

		  /**For BPAs**/
		  WHEN BPAFLG = 1
			 THEN NULL

		  /**For Medication Alerts**/
		  WHEN MedicationFLG = 1
			 THEN CASE WHEN Alerts.MedicationAlertTypeCD = 1 -- 'Drug-Drug'
					   AND Alerts.DrugSeverityDSC IN ('Contraindicated Drug Combination', 'Severe Interaction')
					   AND (Alerts.ProviderTypeNM NOT IN ('Pharmacist', 'Pharmacy Resident')
						OR Alerts.ProviderTypeNM IS NULL)
					   AND Alerts.AlertStatusCD <> 3 -- Filtered
					 THEN 1
				  ELSE 0
				  END
		  END AS PharmacyProviderDrugDrugFLG
		 , Alerts.AlertStatusCD
		 , Alerts.DrugSeverityCD
		 , Alerts.DrugSeverityDSC
		 , CASE 

		  /**For BPAs**/
		  WHEN BPAFLG = 1
			 THEN CASE WHEN(Alerts.ActionTakenCD <> 31 -- 'Accept BPA (No Action Taken)'
					    OR Alerts.ActionTakenCD IS NULL
					    OR Alerts.ActionTakenCD <> 18 -- 'Single Order'
						 )
					  AND Alerts.BestPracticeTriggerActionDSC IN ('General BPA section', 'IP Admission BPA section', 'IP Discharge BPA section', 'IP Rounding BPA section', 'IP Transfer BPA section')
					 THEN 0 WHEN(Alerts.ActionTakenCD <> 31 -- 'Accept BPA (No Action Taken)'
							OR Alerts.ActionTakenCD IS NULL
							OR Alerts.ActionTakenCD <> 18 -- 'Single Order'
							  )
						   AND Alerts.BestPracticeTriggerActionDSC NOT IN ('General BPA section', 'IP Admission BPA section', 'IP Discharge BPA section', 'IP Rounding BPA section', 'IP Transfer BPA section')
					 THEN 1
				  END

		  /**For Medication Alerts**/
		  WHEN MedicationFLG = 1
			 THEN CASE WHEN Alerts.MedicationAlertTypeCD = 1 -- 'Drug-Drug'
					   AND Alerts.DrugSeverityDSC IN ('Contraindicated Drug Combination', 'Severe Interaction')
					 THEN 1 WHEN Alerts.MedicationAlertTypeCD = 6 -- 'Dose'
						    AND Alerts.AlertStatusCD <> 3 -- 'Filtered'
					 THEN 1 WHEN Alerts.MedicationAlertTypeCD = 2 -- 'Drug-Allergy (Active and Inactive Ingredients)'
						    AND Alerts.AlertStatusCD <> 3 -- 'Filtered'
					 THEN 1 WHEN Alerts.MedicationAlertTypeCD = 10 -- 'Duplicate Medication Order'
						    AND Alerts.AlertStatusCD <> 3 -- 'Filtered'
					 THEN 1 WHEN Alerts.MedicationAlertTypeCD = 11 -- 'Pregnancy'
						    AND Alerts.AlertStatusCD <> 3 -- 'Filtered'
					 THEN 1 WHEN Alerts.MedicationAlertTypeCD = 1 -- 'Drug-Drug'
						    AND Alerts.DrugSeverityDSC IN ('Moderate Interaction', 'Undetermined Severity - Alternative Therapy Interaction')
						    AND Alerts.AlertStatusCD <> 3 -- 'Filtered'
					 THEN 0
				  ELSE NULL
				  END
		  END AS InterruptFLG,
		   CASE WHEN Alerts.GraduationYearNBR <> '' AND Alerts.GraduationYearNBR  is not null
			THEN datediff(yy,Alerts.GraduationYearNBR, Alerts.ContactDTS) 
			ELSE null end as YearsExperienceNBR
	   FROM
		   AlpineSubjMrt.CDS.ClinicalDecisionMetricAlertsPre Alerts
		   LEFT JOIN [IDEA].[BPAAlert].[BPAAlert] BPATypes ON Alerts.AlertDSC = BPATypes.AlertDSC
		   LEFT JOIN Epic.Reference.Resource psb ON psb.ProviderID = Alerts.ProviderID COLLATE Latin1_General_CS_AS_KS_WS -- Case-sensitive join
												   --AND psb.LineNBR = 1 -- Primary specialty
		   LEFT JOIN Epic.Encounter.PatientEncounterHospital pehb ON pehb.PatientEncounterID = Alerts.PatientEncounterID
		   LEFT JOIN Epic.Encounter.PatientEncounter peb ON peb.PatientEncounterID = Alerts.PatientEncounterID
		   LEFT JOIN Epic.Finance.Coverage cb ON peb.CoverageID = cb.CoverageID -- Added 3/1/19 as part of Epic 2018 upgrade to replace deprecated columns in PatientEncounter
		   LEFT JOIN Epic.Finance.VCoveragePayorPlan vcb ON cb.Coverageid = vcb.CoverageID
		   LEFT JOIN Epic.Reference.Department dep on dep.DepartmentID = peb.DepartmentID
		   LEFT JOIN Epic.Reference.Location loc on loc.LocationID = dep.RevenueLocationID
		   LEFT JOIN Epic.Finance.HospitalAccount hab ON hab.HospitalAccountID = peb.HospitalAccountID
		   LEFT JOIN Epic.Finance.HospitalAccountDiagnosis hadb ON hadb.HospitalAccountID = hab.HospitalAccountID
													    AND hadb.LineNBR = 1 -- Primary diagnosis
		   LEFT JOIN Epic.Reference.ICD9Diagnosis ib ON ib.DiagnosisID = hadb.DiagnosisID
		   LEFT JOIN Epic.Reference.ICD9Diagnosis ib2 ON ib2.DiagnosisID = ib.ParentDiagnosisID
		   	LEFT JOIN Epic.Reference.Payor pb ON pb.PayorID = CASE WHEN cb.CoverageTypeCD = 2 THEN vcb.PayorID ELSE cb.PayorID END
		   LEFT JOIN Epic.Reference.FinancialClass fcb ON fcb.FinancialClassCD = pb.FinancialClassCD
		   LEFT JOIN Epic.Reference.Department db ON db.DepartmentID = Alerts.PatientDepartmentID
		   LEFT JOIN Epic.Reference.Location lb ON lb.LocationID = db.RevenueLocationID
      --goal=exclude care plan x data
	  where Alerts.seq = 1 AND (loc.HospitalParentLocationID in ('113330', '102229') OR loc.HospitalParentLocationID IS NULL) --Allowing for missing ParentLocationIDs. 
	  ) 



    SELECT 
	    Alerts.*
	  , lr.MaxLineNBR -- This is the most recent documented race
	  , CASE WHEN InterruptFLG = 1
			AND BPAFLG = 1
		   THEN 

	    /**For BPAs**/

	    CASE WHEN(Alerts.ActionTakenCD <> 31 -- 'Accept BPA (No Action Taken)'
			 OR Alerts.ActionTakenCD IS NULL
			 OR Alerts.ActionTakenCD <> 18 -- 'Single Order'
			   )
		    AND Alerts.SpecificOverrideReasonDSC = 'Recommended Action'
		   THEN 1
	    ELSE 0
	    END WHEN InterruptFLG = 0
		    AND BPAFLG = 1
		   THEN 
		CASE WHEN Alerts.SpecificOverrideReasonDSC IS NOT NULL 
			THEN 1
		ELSE 0
		END


	    /**For Medication Alerts**/
	    WHEN MedicationFLG = 1
		   THEN CASE WHEN Alerts.MedicationAlertTypeCD = 1 -- 'Drug-Drug'
				    AND Alerts.DrugSeverityDSC IN ('Contraindicated Drug Combination', 'Severe Interaction')
				    AND Alerts.AlertStatusCD <> 1 -- 'Overridden'
				  THEN 1 WHEN Alerts.MedicationAlertTypeCD = 6 -- 'Dose'
						AND Alerts.AlertStatusCD <> 1 -- 'Overridden'
				  THEN 1 WHEN Alerts.MedicationAlertTypeCD = 2 -- 'Drug-Allergy (Active and Inactive Ingredients)'
						AND Alerts.AlertStatusCD <> 1 -- 'Overridden'
				  THEN 1 WHEN Alerts.MedicationAlertTypeCD = 10 -- 'Duplicate Medication Order'
						AND Alerts.AlertStatusCD <> 1 -- 'Overridden'
				  THEN 1 WHEN Alerts.MedicationAlertTypeCD = 11 -- 'Pregnancy'
						AND Alerts.AlertStatusCD <> 1 -- 'Overridden'
				  THEN 1 WHEN Alerts.MedicationAlertTypeCD = 1 -- 'Drug-Drug'
						AND Alerts.DrugSeverityDSC IN ('Moderate Interaction', 'Undetermined Severity - Alternative Therapy Interaction')
						AND Alerts.AlertStatusCD <> 3 -- 'Filtered'
				  THEN 1
			   ELSE 0
			   END
	    END AS AcceptedFLG
	  , CASE 

	    /**For BPAs**/
	    WHEN InterruptFLG = 1
		AND BPAFLG = 1
		   THEN CASE WHEN(Alerts.ActionTakenCD <> 31 -- 'Accept BPA (No Action Taken)'
					OR Alerts.ActionTakenCD IS NULL
					OR Alerts.ActionTakenCD <> 18 -- 'Single Order'
					  )
				   AND Alerts.SpecificOverrideReasonDSC <> 'Recommended Action'
				   AND Alerts.SpecificOverrideReasonDSC IS NOT NULL
				  THEN 1
			   ELSE 0
			   END WHEN InterruptFLG = 0
				   AND BPAFLG = 1
		   THEN 0
			--CASE WHEN(Alerts.ActionTakenCD <> 31 -- 'Accept BPA (No Action Taken)'
			--		OR Alerts.ActionTakenCD IS NULL
			--		OR Alerts.ActionTakenCD <> 18 -- 'Single Order'
			--		  )
			--	   AND Alerts.SpecificOverrideReasonDSC IS NOT NULL
			--	  THEN 1
			--   ELSE 0
			--   END


	    /**For Medication Alerts**/
	    WHEN MedicationFLG = 1
		   THEN 0
	    END AS ExceptionFLG
	  , CASE 

	    /**For BPAs**/
	    WHEN BPAFLG = 1
		   THEN CASE WHEN(Alerts.ActionTakenCD <> 31 -- 'Accept BPA (No Action Taken)'
					OR Alerts.ActionTakenCD IS NULL
					OR Alerts.ActionTakenCD <> 18 -- 'Single Order'
					  )
				   AND Alerts.SpecificOverrideReasonDSC IS NULL
				  THEN 1
			   ELSE 0
			   END

	    /**For Medication Alerts**/
	    WHEN MedicationFLG = 1
		   THEN NULL
	    END AS IgnoredFLG
	  , CASE 

	    /**For BPAs**/
	    WHEN BPAFLG = 1
		   THEN CASE WHEN(Alerts.ActionTakenCD <> 31 -- 'Accept BPA (No Action Taken)'
					OR Alerts.ActionTakenCD IS NULL
					OR Alerts.ActionTakenCD <> 18 -- 'Single Order'
					  )
				   AND Alerts.SpecificOverrideReasonDSC IS NULL
				  THEN 1
			   ELSE 0
			   END

	    /**For Medication Alerts**/
	    WHEN MedicationFLG = 1
		   THEN CASE WHEN Alerts.MedicationAlertTypeCD = 1 -- 'Drug-Drug'
				    AND Alerts.DrugSeverityDSC IN ('Contraindicated Drug Combination', 'Severe Interaction')
				    AND Alerts.AlertStatusCD = 1 -- 'Overridden'
				  THEN 1 WHEN Alerts.MedicationAlertTypeCD = 6 -- 'Dose'
						AND Alerts.AlertStatusCD = 1 -- 'Overridden'
				  THEN 1 WHEN Alerts.MedicationAlertTypeCD = 2 -- 'Drug-Allergy (Active and Inactive Ingredients)'
						AND Alerts.AlertStatusCD = 1 -- 'Overridden'
				  THEN 1 WHEN Alerts.MedicationAlertTypeCD = 10 -- 'Duplicate Medication Order'
						AND Alerts.AlertStatusCD = 1 -- 'Overridden'
				  THEN 1 WHEN Alerts.MedicationAlertTypeCD = 11 -- 'Pregnancy'
						AND Alerts.AlertStatusCD = 1 -- 'Overridden'
				  THEN 1 WHEN Alerts.MedicationAlertTypeCD = 1 -- 'Drug-Drug'
						AND Alerts.DrugSeverityDSC IN ('Moderate Interaction', 'Undetermined Severity - Alternative Therapy Interaction')
						AND Alerts.AlertStatusCD <> 3 -- 'Filtered'
				  THEN 0
			   ELSE 0
			   END
	    END AS RejectedFLG
    FROM pre Alerts
    LEFT JOIN LastRace lr ON lr.PatientID = Alerts.PatientID


