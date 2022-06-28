----GO

---CREATE VIEW [Views].[BallotTrackLCCCDFScanIn]
	---AS 
SELECT AREA.Area_Code, ISNULL(BT.TotalScanIn, 0) as 'TotalScanIn', ISNULL(BT.TotalCons, 0) as 'TotalCons' FROM [Staging].EMS_VW_EventAreaAuthorityLGWardIan AREA
LEFT JOIN (
	SELECT A.LGACode, ISNULL(B.TotalScanIn, 0) as 'TotalScanIn', ISNULL(A.TotalCons, 0) as 'TotalCons' FROM (
SELECT LGACode, count(*) as TotalCons FROM DV_Staging_BallotTrack_2021.dbo.NSW_Container CONTAINER
		LEFT JOIN DV_Staging_BallotTrack_2021.dbo.NSW_ContainerStatus CONTAINERSTATUS ON CONTAINERSTATUS.ConsKey = CONTAINER.ConsKey AND CONTAINERSTATUS.[LineNo] = CONTAINER.[LineNo]
		LEFT JOIN DV_Staging_BallotTrack_2021.dbo.NSW_Consignment CONSIGNMENT ON CONSIGNMENT.[ID] = CONTAINERSTATUS.[ConsKey]  AND CONTAINERSTATUS.StatusNotes NOT LIKE '%Replaced By%'
		WHERE ContainerName = 'Carton' and ContestName = 'LC'  AND CONTAINERSTATUS.StatusStage = 4.00
		GROUP BY CONSIGNMENT.LGACode
) A

LEFT JOIN
(
--Scanned In Cartons
SELECT LGACode, count(*) as TotalScanIn FROM DV_Staging_BallotTrack_2021.dbo.NSW_Container CONTAINER
		INNER JOIN DV_Staging_BallotTrack_2021.dbo.NSW_ContainerStatus CONTAINERSTATUS ON CONTAINERSTATUS.ConsKey = CONTAINER.ConsKey AND CONTAINERSTATUS.[LineNo] = CONTAINER.[LineNo]
		INNER JOIN DV_Staging_BallotTrack_2021.dbo.NSW_Consignment CONSIGNMENT ON CONSIGNMENT.[ID] = CONTAINERSTATUS.[ConsKey]  AND CONTAINERSTATUS.StatusNotes NOT LIKE '%Replaced By%'
		WHERE ContainerName = 'Carton' and ContestName = 'LC'  AND CONTAINERSTATUS.StatusStage = 5.11
		GROUP BY CONSIGNMENT.LGACode
) B ON A.LGACode = B.LGACode) BT ON BT.LGACode COLLATE DATABASE_DEFAULT = AREA.Area_Code



