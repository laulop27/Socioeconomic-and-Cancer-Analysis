/**
ENUNCIADO:

Los datos representan diferentes características socioeconómicas de distintas regiones, 
extraídas de the American Community Survey (census.gov), clinicaltrials.gov, y cancer.gov.

La variable `deathRate`, nuestra variable objetivo, que representa la mortalidad media de cancer, por cada 100 000 habitantes. 
El resto de variables son:
-   `medianIncome`: mediana de los ingresos de la región.
-   `popEst2015`: población de la región
-   `povertyPercent`: porcentaje de población en situación de pobreza.
-   `studyPerCap`: ensayos clínicos relacionados por el cáncer realizados por cada 100 000 habitantes.
-   `MedianAge`: edad mediana.
-   `region`: nombre de la región.
-   `AvgHouseholdSize`: tamaño medio de los hogares.
-   `PercentMarried`: porcentaje de habitantes casados.
-   `PctHS25_Over`: porcentaje de residentes por encima de los 25 años con (máximo) título de bachillerato.
-   `PctUnemployed16_Over`: porcentaje de residentes de más de 16 años en paro.
-   `PctPrivateCoverage`: porcentaje de residentes con seguro de salud privado.
-   `BirthRate`: tasa de natalidad.
**/

/** CARGAMOS LOS DATOS **/
data no_super.cancer;
    infile "C:\Users\laura\Downloads\APRENDIZAJE_NO_SUPERVISADO\cancer.csv" dsd firstobs=2;
    input deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate; /*NO VOY A CARGAR LA VARIABLE REGION*/ 
    ID = _N_;   /* genera un identificador único con el número de fila */
run;

/***********************************************COMPONENTES PRINCIPALES*********************************************************/

/************** 1º PASO: DETECCIÓN DE OUTLIERS ************/
/* comprobamos si hay datos atipicos llamando a la MACRO*/
%outliers_mult (data=no_super.cancer, var= deathRate medIncome popEst2015 povertyPercent studyPerCap 
MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate, id=ID);

PROC PRINT data=outliers;
where conclusion ne "caso logico"; /*multivariante*/
RUN;

PROC PRINT data= Univariante; 
where (llamada= "PROBLEMAS"); /*univariante*/
RUN;

/*Hemos detectado 33 outliers, por tanto, para ver si estos outliers afectan
signfiicativamente a la media, comparamos medias de con y sin outliers. Si las medias varian mucho serán porque los outliers 
provocan grnades efectos sobre la media y deberemos quitarlos, si no afectan mucho entonces podemos dejarlos porque no estarían 
desplazando la media hacia esos valores atípicos*/

data no_super.cancer_sin_outliers; 
    set no_super.cancer;
    where ID not in ( 
388 194 174 24 124 176 412 222 495 274 
123 492 394 472 210 366 166 98 34 177 
122 260 256 300 113 30 101 182 426 387 
226 319 384); /*cogemos todas las observaciones que no son estos ID*/
run;

/*vamos a comparar las medias para tener una primera vision de las medias (si varian mucho es posible que luego varíen los resultados en los factores)*/
PROC MEANS 
DATA=no_super.cancer_sin_outliers mean NMISS; 
RUN; 

PROC MEANS 
DATA=no_super.cancer mean NMISS; 
RUN; 
/************************************/

/** 2º PASO: ¿Dónde se encuentra el centro de gravedad? **/
PROC MEANS 
DATA=no_super.cancer mean NMISS; 
RUN; 
/**vemos que las medias no son 0, asi que creamos Yi=xi-media**/

/***3º PASO: OBSERVAMOS LA MATRIZ DE CORRELACIONES***/
PROC CORR 
data=no_super.cancer_sin_outliers;
var deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

ods excel file="correlaciones.xlsx";
proc corr data=no_super.cancer_sin_outliers;
var deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;
ods excel close;

/*****/
/*sin outliers*/
PROC PRINCOMP 
DATA=no_super.cancer_sin_outliers   OUTSTAT=Stat  OUT=results;
var deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

/*con outliers*/
PROC PRINCOMP 
DATA=no_super.cancer   OUTSTAT=Stat  OUT=Crime1;
var deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

/*APARTADO 4.3 a)*/
PROC PRINCOMP 
DATA=no_super.cancer_sin_outliers   OUTSTAT=Stat  OUT=results n=5 plots=all;
var deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

/*apartado 4.3 b)*/
PROC PRINCOMP 
DATA=no_super.cancer_sin_outliers   OUTSTAT=Stat n=5 OUT=results plots=all;
var deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
ods output Eigenvalues = autovalores;  
ods output Eigenvectors = autovectores; 
RUN; 

DATA correlaciones (drop = label); 
SET autovectores; 
cor1 = Prin1*sqrt(3.82185605);  
cor2 = Prin2*sqrt(2.37440522); 
cor3 = Prin3*sqrt(1.13030207); 
cor4 = Prin4*sqrt(1.05132283); 
cor5 = Prin5*sqrt(0.86155580); 
RUN; 

PROC PRINT DATA=correlaciones;
VAR Variable cor1--cor5;
RUN; 

/************************************************************* ANÁLISIS FACTORIAL ****************************************************************/

/*APARTADO 5.1)*/
PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  
METHOD=PRINCIPAL PRIORS=ONE REORDER RESIDUAL;
	VAR deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  
METHOD=PRINCIPAL PRIORS=ONE REORDER RESIDUAL n=5;
	VAR deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

/*APARTADO 5.2)*/
PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  
METHOD=PRINCIPAL PRIORS=ONE REORDER RESIDUAL n=5;
	VAR deathRate medIncome popEst2015 povertyPercent /*studyPerCap*/ MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ANÁLISIS FACTORIAL CON MÉTODO DE COMPONENTES PRINCIPALES <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/
/* partimos de: */
title 'C.P N=5';
PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  
METHOD=PRINCIPAL PRIORS=ONE REORDER RESIDUAL n=5;
	VAR deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

title 'C.P N=6';
PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  
METHOD=PRINCIPAL PRIORS=ONE REORDER RESIDUAL n=6;
	VAR deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

title 'C.P N=7';
PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  
METHOD=PRINCIPAL PRIORS=ONE REORDER RESIDUAL n=7;
	VAR deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

/*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ANÁLISIS FACTORIAL CON MÉTODO DE FACTORES PRINCIPALES <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  EIGENVECTORS
METHOD=PRINIT PRIORS=ONE REORDER RESIDUAL MAXITER=100 N=4;
	VAR deathRate medIncome popEst2015 povertyPercent studyPerCap MedianAge 
        AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

title 'F.P n=4';
PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  EIGENVECTORS
METHOD=PRINIT PRIORS=ONE REORDER RESIDUAL MAXITER=100 N=4;
	VAR deathRate medIncome popEst2015 povertyPercent /*studyPerCap*/ MedianAge 
        AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

title 'F.P n=4';
PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  EIGENVECTORS
METHOD=PRINIT PRIORS=ONE REORDER RESIDUAL MAXITER=100 N=4;
	VAR deathRate medIncome /*popEst2015*/ povertyPercent /*studyPerCap*/ MedianAge 
        /*AvgHouseholdSize*/ PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage /*BirthRate*/;
RUN;


/*nos quedamos con componentes principales (PRINCIPAL) porque las comunalidades finales son mejores con el mismo nº de factores , 
porq con prinit hay 2 variables que no conseguimos explicar*/

/*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ANÁLISIS FACTORIAL CON MÉTODO DE MÁXIMA VEROSIMILITUD <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/
/*ANTES DE HACER ML HAY QUE MIRAR SI SON NORMALES CON LA MACRO*/
PROC FACTOR data=no_super.cancer  SIMPLE MSA  
METHOD=ML PRIORS=ONE REORDER RESIDUAL n=5 MAXITER=150 HEYWOOD
	/*OUT=FACTORES OUTSTAT=Estad;*/;
	VAR deathRate medIncome popEst2015 povertyPercent /*studyPerCap*/ MedianAge /*AvgHouseholdSize*/ PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

PROC FACTOR data=no_super.cancer  SIMPLE MSA  
METHOD=ML PRIORS=ONE REORDER RESIDUAL n=4 MAXITER=150 HEYWOOD
	/*OUT=FACTORES OUTSTAT=Estad;*/;
	VAR deathRate medIncome popEst2015 povertyPercent /*studyPerCap*/ MedianAge /*AvgHouseholdSize*/ PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

/*AHORA QUEDARIA ROTAR EL DE ML Y DARLE NOMBRE A CADA FACTOR*/
%NORMAL_MULT(DATA=no_super.cancer_sin_outliers, var= deathRate medIncome popEst2015 povertyPercent studyPerCap 
MedianAge AvgHouseholdSize PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate); 

PROC FACTOR data=no_super.cancer  SIMPLE MSA  
METHOD=PRINCIPAL PRIORS=ONE REORDER RESIDUAL n=4 MAXITER=150 HEYWOOD ROTATE=varimax
	OUT=FACTORES OUTSTAT=Estad;
	VAR deathRate medIncome popEst2015 povertyPercent /*studyPerCap*/ MedianAge /*AvgHouseholdSize*/ PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage BirthRate;
RUN;

/********* COMPARACIÓN DE MODELOS **********/
title 'F.P n=7';
PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  EIGENVECTORS
METHOD=PRINIT PRIORS=ONE REORDER RESIDUAL MAXITER=100 N=7;
	VAR deathRate medIncome /*popEst2015*/ povertyPercent /*studyPerCap*/ MedianAge 
        /*AvgHouseholdSize*/ PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage /*BirthRate*/;
RUN;

/*metodo seleccionado: */
title 'PRINIT';
PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  EIGENVECTORS
METHOD=PRINIT PRIORS=ONE REORDER RESIDUAL MAXITER=100 N=4 plots=all;
	VAR deathRate medIncome /*popEst2015*/ povertyPercent /*studyPerCap*/ MedianAge /*AvgHouseholdSize*/ PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage /*BirthRate*/;
RUN; 

/******* ROTACION *******/
PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  EIGENVECTORS
METHOD=PRINIT PRIORS=ONE REORDER RESIDUAL MAXITER=100 N=4 plots=all ROTATE=VARIMAX heywood scree;
	VAR deathRate medIncome /*popEst2015*/ povertyPercent /*studyPerCap*/ MedianAge /*AvgHouseholdSize*/ PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage /*BirthRate*/;
RUN; 

PROC FACTOR data=no_super.cancer_sin_outliers  SIMPLE MSA  EIGENVECTORS
METHOD=PRINIT PRIORS=ONE REORDER RESIDUAL MAXITER=100 N=4 plots=all ROTATE=QUARTIMAX;
	VAR deathRate medIncome /*popEst2015*/ povertyPercent /*studyPerCap*/ MedianAge /*AvgHouseholdSize*/ PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage /*BirthRate*/;
RUN; 

/********************************************************** CORRESPONDENCIAS SIMPLES *************************************************************/
/*apartado 6 del trabajo*/
PROC FACTOR data=no_super.cancer_sin_outliers  
			SIMPLE MSA METHOD=PRINIT PRIORS=ONE REORDER RESIDUAL n=4 MAXITER=100 HEYWOOD ROTATE=varimax
			OUT=FACTORES OUTSTAT=Estad;

	VAR deathRate medIncome /*popEst2015*/ povertyPercent /*studyPerCap*/ MedianAge /*AvgHouseholdSize*/ PercentMarried PctHS25_Over PctUnemployed16_Over PctPrivateCoverage /*BirthRate*/;
RUN; 
proc print data=no_super.cancer_sin_outliers;run; 

proc univariate data=FACTORES noprint;
   var Factor1  AvgHouseholdSize; /*usamos un Factor y una de las variables que hemos quitado*/
   output out=Percentiles PCTLPTS=25 50 75  
PCTLPRE=Factor1_ AvgHouseholdSize_  PCTLNAME=P25 P50 P75;
RUN;
proc print data=Percentiles;run;

/*transformamaos la variable continua a categorica en base a los percentiles*/
DATA correspondencias;
	SET FACTORES;
	IF Factor1 < = -0.66201 THEN Factor1_REC = 1; /*asiganará 1 si pertence al 1º cuartil*/
	IF -0.66201 < Factor1 <= -0.054362 THEN Factor1_REC = 2; /*asiganará 2 si pertence entre el 1º y 2º cuartil*/
	IF -0.054362 < Factor1 <= 0.57481 THEN Factor1_REC = 3; /*asiganará 3 si pertence entre el 2º y 3º cuartil*/
	IF Factor1 > 0.57481 THEN Factor1_REC = 4; /*asiganará 4 si pertence entre el 3º cuartil*/
	IF Factor1 = . THEN Factor1_REC = .; 
RUN;
PROC PRINT; RUN;

DATA correspondencias;
	SET correspondencias;
	IF AvgHouseholdSize < = 2.31 THEN AvgHouseholdSize_REC = 11; /*asiganará 1 si pertence al 1º cuartil*/
	IF 2.31 < AvgHouseholdSize <= 2.44 THEN AvgHouseholdSize_REC = 12; /*asiganará 2 si pertence entre el 1º y 2º cuartil*/
	IF 2.44 <  AvgHouseholdSize <= 2.59 THEN AvgHouseholdSize_REC = 13; /*asiganará 3 si pertence entre el 2º y 3º cuartil*/
	IF AvgHouseholdSize > 2.59 THEN AvgHouseholdSize_REC = 14; /*asiganará 4 si es mayor que el 3º cuartil*/
	IF AvgHouseholdSize = . THEN AvgHouseholdSize_REC = .; 
RUN;
PROC PRINT DATA=correspondencias; RUN;

/*formateamos para categorizarla y aplicarle el formato*/
PROC FORMAT;
VALUE  Factor1_REC 1='Factor_socieconomico_MuyBajo ' 2='Factor_Socieconomico_Bajo ' 3='Factor_socieconomico_Medio ' 4='Factor_socieconomico_Alto'  ;
VALUE  AvgHouseholdSize_REC 11='Tamaño_MuyPequeño ' 12='Tamaño_Medio ' 13='Tamaño_Alto ' 14='Tamaño_Muygrande'  ;
run;

data no_super.correspondencias1;
set correspondencias;
format Factor1_REC Factor1_REC. ;
format AvgHouseholdSize_REC AvgHouseholdSize_REC. ;
run;
PROC PRINT DATA=no_super.correspondencias1; RUN;

/*ahora tenemos dos tablas de contingencia, hemos pasado de 2 variables cuantis a dos cualis*/
PROC FREQ DATA=no_super.correspondencias1;  /*vamos a ver si la chi cuadrado es o no significativa*/
TABLES Factor1_REC AvgHouseholdSize_REC; RUN;

PROC FREQ DATA=no_super.correspondencias1; 
TABLES Factor1_REC*AvgHouseholdSize_REC; RUN;

/* Realizar una prueba de chi-cuadrado utilizando PROC FREQ, vemos uqe NO son independientes */
PROC FREQ DATA=no_super.correspondencias1;
tables Factor1_REC * AvgHouseholdSize_REC / chisq plots=freqplot(type=dotplot scale=percet /*log sqrt*/);
run;

/*1ª PREGUUNTA: sabiendo q factor1 es < q1, donde está en AvgHouseholdSize o como es la AvgHouseholdSize
esta pregunta se resuelve con las CONDICIONADAS, estas condicionadas serán n_11/n_1i, n_12/n_1i, n_13/n_1i, n_14/n_1i, esto serán
los perfiles fila*/

/*PERFILES FILA (RF), PERFILES COLUMNA (RC) Y VARIABLES Y*/
proc iml;
RESET PRINT;
N={26 24 38 37,
22 48 35 20, 
33 37 30 25,
52 17 16 40};/*metemos los valores de la tabla Factor1_REC*AvgHouseholdSize_REC (la 1ª fila de cada una)*/
NOBS={1 1 1 1}*N*{1,1,1,1};
/*FRECUENCIAS RELATIVAS Y MARGINALES DE FILAS (MF) Y DE COLUMNAS(MC)*/
F=N/NOBS;
MF=F*{1,1,1,1};
MC=T(F)*{1,1,1,1};
/*NE=FRECUENCIAS ESPERADAS, ERRORES, COMPONENTES DEL ESTADSTICO CHI-CUADRADO E INERCIA */
NE=MF*T(MC)*NOBS;
ERROR=N-NE;
CHI=(ERROR##2)/NE;

TCHI=SUM(CHI); /*valor de la chi--> si TCHI > Valor_crit rechazamos H0 de independencia*/
Valor_crit=quantile('CHISQ', 0.95, 9); /* Imprime el valor crítico, el 9 son los grados de libertad de la chi*/

/*
TCHI = 52.548146
Valor_crit = 16.918978
TCHI > Valor_crit --> rechazamos la H0, rechazamos independencia
*/

/*si todas las modas son iguales, significa que independientemente de donde esté Factor1, la variable AvgHouseholdSize
siempre tomará el mismo valor y no tienen sentido hacer un análisis de correspondencias simples*/

/*analisis de correspondencias para ver la dependencia de los ptos. filas, la de los ptos. columna o para un estudio simétrico
para ello vamos a fijar, dpeendiendo de lo que queramos fijaremos lo ptos. columna o ptos. fila (los ptos son la frecuencia relativas condicionadas)*/

/**/
PROC CORRESP DATA=no_super.correspondencias1 all chi2p OUTC=GRAFICA 
PROFILE=ROW; /*usamos perfiles fila, porq nos interesa ver q es lo q pasa con la confianza sabiendo el aspecto q tiene*/
TABLES Factor1_REC, AvgHouseholdSize_REC;
RUN; 


/*si todas las modas son iguales, significa que independientemente de donde esté Factor1, la variable AvgHouseholdSize
siempre tomará el mismo valor y no tienen sentido hacer un análisis de correspondencias simples*/

/*PARA VER SI HAY INDEPENDENCIA HACEMOS EL TEST DE LA CHI-CUADRADO*/







