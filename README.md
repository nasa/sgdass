# 0.0   Preface.

      This document provides a brief description of the Space geodesy data 
      analysis software suite maintained by NASA
 
      Author of this document: Leonid Petrov

      Last modification:       2025.01.18

# 1.0   Introduction.


      Space geodesy data analysis software suite (SGDASS) is a collection 
      of software programs under Linux and MACOS operating systems
      with the primary goal of analysis of very long baseline interferometry 
      (VLBI) observations. It provides a layer of software of a pipeline for 
      analysis of VLBI data for both research and development purposes and 
      for generation of data products on a regular basis in a form suitable 
      for scientific publications and their dissemination to the scientific 
      community.


## 1.1.  Structure of the software package


      Scientific analysis layer  
      =========================
                          |
                          |---- Processing results of parameter estimation
                          |
                          |---- Parameter estimation of group delays
                          |
                          |---- Simulation of space geodesy experiments
                          |
                          |---- Computation of theoretical path delay,
                          |     its derivatives, and Doppler shift
                          |
                          |---- Offline computation of a priori model
                          |     parameters used for data analysis
                          |
                          |---- Processing raw VLBI visibility data
                          |
                          |---- Scheduling VLBI observations 
      
      
      Layer of numerical methods implementation
      =========================================
                          |
                          |---- Fast spherical harmonics transform
                          |
                          |---- B-spline expansion routines
                          |
                          |---- Chebyschev polynomial expansion routines
                          |
                          |---- Legendre polynomial expansion routines
                          |
                          |---- Linear algebra routines
                          |
                          |---- Linear regression routines
                          |
                          |---- Time transformation routines
                          |
                          |---- Geodetic transformation routines
      
      
      I/O support level
      =====================
                          |
                          |---- Geo VLBI database handler
                          |
                          |---- Support of automatic services for retrieving
                          |     data from external servers that are required
                          |     for data analysis
                          |
                          |---- Interface to cfitsio library for read/write
                          |     data in FITS-format
                          |
                          |---- Interface to Unix low level I/O routines
      
      
      Graphic support level
      =====================
                          |
                          |---- Library of the Dialogue graphic interface
                          |
                          |---- Tools for 2D visualization
      
      
      
      
      Interface to operating system support level
      ===========================================
                          |
                          |---- Interface to operating system support level
      
      
## 1.2.  Dependencies on third party open source packages, including
      indirect dependencies:
      
      autoconf
      automake
      bison
      bzip2
      cfitsio
      cmake
      curl
      fftw
      flex
      gcc
      gmp
      hdf
      hdf5
      isl
      lbzip2
      libjpeg-turbo
      libtirpc
      libtool
      lzlib
      m4
      mpc
      mpfr
      nasm
      ncurses
      netcdf-c
      netcdf-fortran
      openblas
      openldap
      openssl
      parallel
      patch
      pgplot
      pkg-config
      readline
      w3lib
      wget
      xz
      zlib
      zstd


# 2.0.  Scientific analysis layer  

      
## 2.1.  Processing results of parameter estimation.

      Parameter estimation results are delivered in a form of a large
      so-called spool file that can have more than 10 million lines.
      SGDASS provides a number of programs that processed the raw
      spool file and generate data products. This is done in several
      steps. Firstly, program getpar extracts information from an input 
      spool file and writes it into 21 different files. These files 
      provide station positions, station linear velocities, sine and 
      cosine amplitudes of harmonic station position estimates, 
      coefficients of B-spline expansions of stations positions; source 
      coordinates and proper motions; time series of Earth orientation 
      parameters and their time derivatives, coefficients of an expansion 
      of the Earth orientation parameters into the Fourier basis, 
      coefficients of an expansion of the Earth orientation into the 
      B-spline basis; estimates of clock function; estimates of the 
      atmospheric path delays in zenith direction; estimates of baseline 
      clock adjustments.
  
      SGDASS provides a set of programs for transformation of the Earth
      orientation parameters, their comparison, filtering for outliers,
      and smoothing.
  
      SGDASS provides programs for an update of a priori catalogues of source
      coordinates, stations positions, and station velocities for the results
      of parameter estimation. 
  
      SGDASS provides programs for analysis of baseline length repeatability
      repeatabilities of arcs between sources. 
  
      SGDASS provides programs for visualization of results of data analysis,
      such as baseline length repeatabilities, comparison of the Earth
      orientation parameter series for both visual inspection and inclusion
      into scientific publications.
  
## 2.2.  Parameter estimation of group delays

      The primary goal of data analysis is to provide robust and accurate 
      estimates of parameters of the model of geodetic observations as well
      as realistic estimates of their uncertainties. Theoretical values of 
      group delays and their derivatives are computed, and small differences 
      between observed and calculated delays, called o-c are formed. Bad data 
      are identified  and flagged. Linear equations of the dependence on o-c 
      on parameters of the model are formed. These equations are augmented 
      with equations of constraints. The final system of equations is solved 
      using multi-group least squares, and the variance-covariance matrix is 
      computed in addition to computation of the vector of estimates. Since 
      the number of parameters may exceed one million, an efficient method 
      for solving a large sparse problem with a regular portrait is 
      implemented.
 
      Parameter estimation software works in two modes: preprocessing and 
      final analysis. Preprocessing can be done either interactively or 
      inattentively by execution of control file. Final analysis is always
      done in the non-interactive mode. 

### 2.2.1 Preprocessing mode.

      Preprocessing involves three major steps: a) parameterization;
      b) outliers elimination; c) weights update. In a case if visibility 
      analysis was performed by third-party software that used obsolete
      algorithms, an addition step, group delay ambiguity resolution,
      is required.

      Parameterization involves specifying the list of parameter that are 
      estimated: source coordinates, station positions, Earth orientation 
      parameters, clock function, atmospheric path delay in zenith directions,
      tilts of the atmosphere symmetry axis, baseline dependent clocks,
      and clock breaks. Parameterization also involves specifying constraints
      imposed on parameters. Some combinations of estimated parameters and
      constraints result in a singular system of equations. Software provides 
      singularity checks and signals if it finds deficient parameterization.
     
      Outlier elimination involves a procedure that finds an observation
      with the highest normalized residual and implements an update of the 
      normal matrix and normal vector by using the lemma of inversion
      of an augmented matrix. A result of this update is the solution 
      of the least square problem without a given observation. The process
      is iteratively repeated till the specified condition is met. 
      An algorithm for an inverse problem, i.e. update of the least square 
      solution for inclusion of a new observation with the smallest 
      normalized residual is implemented as well. Both the direct and inverse
      outlier elimination procedures are run several times till convergence
      is detected.
           
      The weight update procedure finds baseline-dependent additive parameters
      that, being added in quadrature to the a priori weights, make the ratio
      of the weighted sum of residuals to its mathematical expectation close
      to unity. These additive weight corrections are stored and applied during
      a final geodetic solution. 

      In processing group delays produced by the third-party software, 
      a situation when group delays have ambiguities may happen. That means
      that delay has an additional term in a form of n*sp where sp is the 
      parameter of the ambiguity spacing and n is a random number. In a case
      if n is not constant, this term introduces very large errors in geodetic
      analysis which make results close to useless. SGDASS implements routines 
      for the automatic determination of that random number "n" and correcting 
      affected data.

      Results of preprocessing are stored in the database with data.

### 2.2.2 Final analysis 

      Final data analysis is performed in the batch (non-interactive) mode. 
      The data analysis tool ingests two control files and the list of
      sessions that have to be processed. The first control file defines
      procedures for computation of theoretical delays. The second file
      controls the parameter estimation procedure.

      The control file for computation of theoretical delays specifies names 
      of the models applied, and either parameters of the models or files
      with such parameters. These parameters include stations positions, 
      stations velocities, station eccentricity file, source coordinates, 
      source proper motion or the ephemeride, planetary ephemeride,
      leap second file, coefficients of the empirical Earth rotation model,
      coefficients of the harmonic variations in the Earth rotation,
      time series of the Earth rotation, coefficients of the expansions
      of Earth's rotation into the B-spline basis, coefficients of the 
      expansion of the tide-generating potential, parameters of the mean
      pole tide, time series of station displacements caused by atmospheric 
      pressure loading, time series of station displacements caused by land 
      water storage loading, time series of station displacements caused by 
      non-tidal ocean loading, coefficients of station displacements caused by 
      tidal ocean loading, time series of expansion of slant path delay into  
      the B-spline basis, time series of global ionospheric models, parameters
      of the antenna thermal deformations, and images of extragalactic 
      radio sources.
     
      The control file allows to specify the model for the Earth orientation
      parameters, the model for tidal displacements, the model for solid Earth
      tides, the model for the pole tide, the model of antenna axis offset, 
      the model of slant path delay and its derivatives, the model of
      the path delay in the ionosphere, the relativity model, model of galactic
      aberration, and the source structure model.

      The parameter estimation model defines the rules for the automatic 
      selection of estimated parameters, as well as their scope; defines the 
      rules for forming equations of constraints; defines the mathematical 
      procedure for parameter estimation; defines reweighing rules, defines 
      the list of experiments to process; and defines options for the output
      of parameter estimation. In addition, the control file defines the 
      so-called user program for computation of corrections to the theoretical 
      path delays partials derivatives, and constraints. These user programs 
      work as plug-ins and change the models, add new estimated parameters and 
      new constrains with respect to those that are built in SGDASS. This mode 
      is designed primarily for the research and development use.


## 2.3.  Simulation of space geodesy experiments


      SGDASS provides a tool for simulation of geodetic VLBI observation. 
      The input for the simulation tool is either an existing database
      with VLBI data or a VLBI schedule in vex format.
 
      The simulation tool can work in two modes: simulating observables in 
      the existing experiment and simulating observables in a new experiment.
      In the first mode it takes existing database with an experiment and 
      replaces observed group delays with the modeled ones. In the second 
      mode the simulation tool takes a schedule file, computes group 
      delays, and adds noise. In both cases the added noise is computed 
      using random number generator. The simulation tool allows to 
      generate a Gaussian non-correlated noise with the specified second 
      moment or a stationary Gaussian correlated noise. In the latter case 
      the a priori generating station-dependent autocorrelation is supplied 
      by the user. That autocorrelation is computed by processing the output
      of numerical weather models.
      
      Existing data analysis software processes simulated databases the same
      way as databases with real data.


## 2.4.  Computation of theoretical path delay, its derivatives 
      and Doppler shifts

 
      SGDASS provides a library for computation of theoretical path delay
      and its partial derivatives as as as Doppler shift from the emitter
      to the receivers. The emitter can be an extragalactic object or 
      a satellite. The receiver can be either a ground station or a satellite.
      Accuracy of computation is 0.1 mm. Accuracy of results is limited by
      the accuracy of the a priori models. The largest error source is
      mismodeling of path delay in the neutral atmosphere. Errors in a priori
      atmospheric path delay strongly depend on elevation, and on average, 
      contribute at a level of 30 mm, although at elevation 7 deg they can 
      reach 400 mm under extreme weather conditions.
 
      SGDASS implements the state-of-the art algorithms for computation of
      group delay based on the general relativity paradigm. It applies data
      reduction for the Earth rotation, including luni-solar and planetary
      nutation for an inelastic, elliptical Earth with the solid mantle and 
      liquid core, geodesic nutation, high-frequency variations in the polar
      motion and UT1, variations in UT1 caused by zonal tides; station
      displacements caused by pole tide, solid Earth tides; station caused
      by mass loading due by variations in the atmospheric pressure, land 
      water storage, and ocean bottom pressure; slant path delay in the
      neutral atmosphere; slant path delay in the ionosphere; galactic 
      aberration; source structure; source proper moption; moption of the 
      object presented as an ephemeride or a set of two line element 
      parameters; antenna axis offset; antenna gravity deformation; and 
      compiling effects between the atmospheric path delay and the antenna offset.
 
      Routines of the library for computation of theoretical path delay are 
      used in all parts of the SGDASS: for scheduling, analysis of 
      interferometric visibility, analysis of group delays, and analysis of 
      results of parameter estimation.
 

## 2.5.  Offline computation of a priori model parameters used 
      for data analysis


### 2.5.1 Some data reductions are computed on-the-fly during data analysis, and 
      some data reductions are computed offline, because otherwise, data 
      analysis would take so much time that would have been impractical. The 
      offline a priori model parameter computation includes a) computation of 
      time series of 3D displacements of observing stations caused by of mass 
      loading using the output of numerical weather models; b) computation of 
      time series of gridded slant path delay through the neutral atmosphere; 
      c) computation of the atmospheric angular momentum using the output 
      of numerical weather  models; d) computation of atmospheric opacity in 
      microwave range; e) computation of atmospheric brightness temperature, 
      and f) computation of the Earth orientation parameters, including 
      prediction.
 
      All these computation include processing the output of numerical weather
      model. SGDASS provides an infrastructure for retrieval of the output
      of numerical models on a regular basis in the automated fashion, 
      preprocessing the ingested data files, storing, and recovery from errors.
      Preprocessed outputs of numerical weather model is used in three ways:
      a) computation of the atmospheric angular momentum based inf the wind 
      field and the surface pressure field; b) computation of the surface 
      pressure of the atmosphere, land water storage, and the ocean bottom 
      pressure; c) computation of the 3D field of refractivity, atmospheric 
      opacity, and air temperature. These quantities are used for further 
      computations.
      
### 2.5.2 The atmospheric angular momentum is a sum of the motion term and the
      pressure term. The motion term is computed by expanding the 3D wind field
      in the B-spline basis and then evaluation of the integral of the 3D shell
      using this expansion. The pressure term is derived from the surface 
      atmospheric pressure computed from the expansion of the 3D pressure
      field and interpolating on the physical Earth surface defined by a digital 
      elevation model.
      
      The Earth orientation parameters are predicted to 72 hours in the future 
      from the epoch of the last run of the assimilation model. The predicted 
      value is computed using an auto-regressive model based on prior time
      series of the Earth orientation parameters and the atmospheric angular 
      momentum evaluated from the output of the numerical weather model that 
      correspond to the prediction.
 
      The Earth orientation parameters series are split into four parts:
      1) old parameters that corresponds to epochs 35 days in the past and 
      older; 2) fresh parameters that corresponds to epochs from 35 days in
      the past till the last epoch of Earth orientation parameters derived from
      space geodesy observations; 3) a prediction part since the last epoch of 
      Earth orientation parameters derived from space geodesy observations till
      72 hours in the future; 4) a prediction part for epochs since 72 hours in 
      the future to 180 days in the future. The resulting Earth orientation 
      parameters series from the first part is based on the series IERS C04 
      maintained by Paris observatory; the series from the second part are 
      based on the expansion of existing estimates of the Earth orientation 
      parameters over the B-spline basis; the third part is based on a model 
      that assimilates the aggression from the exiting the Earth orientation 
      parameter series and the atmospheric angular momentum; and the fourth 
      part is based on the IERS C04 Earth orientation parameter series 
      forecast. All parts are matched together to avoid discontinuities. 
      The derived series are expanded into the B-spline basis. These series 
      and the list of coefficients of the harmonic variations in the Earth 
      rotation derived from analysis of observations form a so-called Network
      Earth Rotation Service message. That message is posted on an http 
      server and is automatically updated each time new data are arrive, 
      3 to 8 times a day.
 
      SGDASS contains a library that automatically retrieves the Network Earth 
      Rotation Service message, parses it, and uses the parsed message for
      computation of the Earth rotation matrix for space geodesy data analysis.
 
### 2.5.3 SGDASS contains code for computation of mass loading caused by 
      atmospheric pressure variations, changes of the land water storage, and 
      changes in bottom pressure due to the ocean circulation and tides. That 
      code reuses the infrastructure for retrieval of the output of numerical 
      models. Evaluation of mass loading is done in two steps: a) computation 
      of the surface pressure of air, land water, and ocean water, and 
      b) computation of loading caused by the variable part of that pressure. 
      The variable part of the surface pressure is computed by subtracting 
      a parametric model that includes the mean value, trend, and harmonic 
      constituents. The model is derived from analysis of long series of 
      surface pressure.
 
      Displacements caused by mass loading are computed using the spherical
      harmonics transform approach. The surface pressure is upgraded to 2'x2'
      gird, multiplied by the band-limited sea-level mask, passed to 
      the direct spherical harmonics transform, scaled by appropriate Love 
      numbers, and finally passed to the inverse spherical harmonics transform.
      Then the 2D field of displacements is passed to the routine for sampling
      correction that substially mitigates errors caused by the leakage of 
      the spherical harmonic transform near the coastal line. This 2D field 
      of displacements is further expanded into the B-spline basis, and the 
      expansion coefficients are stored. Based in these coefficients, the time 
      series of displacements in up, east, and north direction for over 1000 
      space geodesy sites is calculated. The 3D displacements, as well Stokes 
      coefficients of the contribution of the atmosphere, land water, storage,
      and oceanic bottom pressure variations to the geopotential are uploaded 
      to the http-server for distribution. SGDASS provides code for 
      an automatic update each time when new data arrive, 3 to 8 times a day. 
      Expansion coefficients are put on the the http server.
 
      SGDASS contains a library that automatically retrieves the time series 
      of geodetic station displacements caused by loading, transforms them 
      into binary formats, and organizes them in a form suitable for data
      reduction. Data reduction library uses these coefficients for 
      interpolation of displacements at a given station, given elevation, 
      given azimuth, and given time.
 
### 2.5.4 Slant path delay in the neutral atmosphere is computed from the output
      of numerical weather models. This is done in four steps. First,
      the a hypsometric differential equation is solved based on the raw data
      in the output, and the state of the atmosphere on a 3D grid is computed.
      Second, based in the state of the atmosphere, the 3D refractivity field
      is computed and expanded over 3D B-spline basis. Third, slant path delay 
      at each station at a given grid of azimuth and elevation is computed by 
      solving differential equations of radio wave propagation in the 
      heterogeneous media. Since the refractivity field is represented in 
      a form of expansion over the B-spline basis, the differential equations 
      are transformed to a system of non-linear algebraic equations that 
      relates the expansion coefficients. This system  is solved by iterations. 
      The solution of these equations provides a curved trajectory of the 
      microwave wavefront in the atmosphere. Fourth, path delay is computed by 
      integration of refractivity over the trajectory. In addition, the field 
      of the atmosphere absorption in the frequency range 1 to 360 GHz is 
      computed in a similar was as the refractivity field and expanded over 
      the 3D B-spline basis.

      SGDASS provides code for integration of the atmospheric absorption
      along the trajectory, which allows to evaluate atmospheric opacity. 
      Based on opacity changes along the wave front trajectory, the equation 
      of the radiative transfer are solved under condition of the local 
      thermal equilibrium. Their solution provides the atmospheric brightness 
      temperature. 

      SGDASS provides code for computation of slant path delay in moist 
      the atmosphere, opacity, and atmospheric brightness temperature on 
      a grid of azimuths and elevation. SGDASS provides code for an automatic 
      update for a list of 299 stations each time when new data arrive, 
      3 to 8 times a day. The expansion coefficients are put on the the http 
      server.
 
      SGDASS contains a library that automatically retrieves the time series 
      of gridded slant path delays, transforms them into binary formats, 
      and organizes them in a form suitable for data reduction.
      Data reduction library uses these coefficients to interpolate slant
      path delay at a given station, given elevation, given azimuth,
      and given time.

   
## 2.6   Processing raw VLBI visibility data


      The primary goal of processing VLBI visibility data is to estimate
      group delays and phase delay rates from time series of cross- and 
      auto-correlations and perform calibrations. Visibility analysis 
      software is controlled by a configuration file. The configuration file 
      defines general parameters and parameters specific for a given task. 
      The general parameters are the experiment code; experiment bands; lists 
      of observations to process; names of files with visibilities; name
      of the staging directory; files with names of observed sources and 
      participating stations; flags for using measured phase calibration;
      flags for using measured system temperature; flags of using use gain 
      calibration, sampler calibration, autocorrelation calibration, 
      amplitude fudge factor, amplitude edge window calibration, amplitude 
      off-beam calibration, autocorrelation normalization, bandpass 
      normalization, bandpass normalization range, and a priori gain 
      calibration. Control file also defines parameters of all the tasks 
      that SGDASS performs with visibility data, file name of a control file 
      for computation of theoretical path delays, names of output files, 
      the tolerance to the jitter in time tags, the minimal scan length, the
      maximum scan length, the maximum gap in data within a given scan, 
      fringe reference time offset, the name of the reference station, the 
      name of the file with indices of excluded visibilities, the name of 
      the file with indices of excluded observations, the name of the file 
      with indices of included observations, the bandpass use flag, the 
      bandpass file name, the bandpass correlation file name, the polarization 
      bandpass correlation file name; the phase calibration mask file, the 
      bandpass mask file, the type of an external interferometric model, 
      the directory with the interferometric model, the lower intermediate 
      frequency index, the intermediate frequency index, the frequency group 
      index, and polarization name.

      Visibility data analysis involves a number of tasks.

### 2.6.1 Data ingestion

      SGDASS parses input files, forms various objects, sorts them, and creates
      hash tables of indices. This operation is performed once, the index 
      tables are stored and later they are used for analysis. SGDASS neither 
      alters input visibility data nor re-formats them. At the second step 
      software loads external calibration information, such as system 
      temperature and antenna gain curves. Then SGDASS performs through checks 
      data self-consistency and flags those data points that fail checks,
      for instance, the visibilities not-a-number or the frequency out of 
      range, or the cross-correlation points without corresponding 
      auto-correlations.

      SGDASS splits the data into segments called scans using the built-in
      algorithm that does not require external information. This process is 
      guided by several parameters in the control file. A portion of data for 
      a given scan at a given baseline is called observation. SGDASS creates 
      a list of scans, a list of observations, a list of observed sources,
      a list of observing stations, and the list of epochs of observations.

### 2.6.2 Fringe fitting 

      Fringe fitting task determines group delay, phase delay, phase delay 
      rate, and optionally, phase acceleration or group delay rates from 
      a given observations. The problem of evaluating a group delay and 
      phase delay rate is strongly non-linear. Therefore, fringe fitting 
      is performed in two steps: coarse fitting and fine fitting. SGDASS 
      performs two types of calibration of visibility data: pre-fit 
      calibration and post-fit calibration. Pre-fit calibration is non-linear. 
      It includes phase calibration, amplitude calibration, and fringe rotated 
      for the contribution of the nonlinear correction to the a priori path 
      delay.

      The core of the coarse fringe fitting procedure is the 2D Fourier 
      transform. The data are mapped into a regular grid padding unused grid 
      with nodes. Then the maximum of the visibility Fourier transform is 
      sought. The coordinates of the grid point of the maximum Fourier 
      transform of gridded visibility data provide coarse estimates of 
      phase delay, group delays, and phase delay rate. In the second step 
      coarse delays and rates are refined using one of the flavors of the 
      linear least square algorithm. The algorithm performs an additive and 
      multiplicative update of a priori weights based on fringe amplitude 
      to account for calibration errors in computation of uncertainties
      of group delays and phase delay rates. During that step the amplitude 
      of the fringe phase noise is computed by sampling a large number of 
      constituents of the Fourier transform of visibilities filtered for 
      points with the signal from the observed source. Finally, post-fit 
      calibrations are applied, such as sampler correction, band and time 
      smearing calibration, and antenna beam calibration.
     
      The parameters in control file that are specific to the visibility 
      data analysis are the name of the coarse search algorithm name, 
      the group delay search window central delay, the group delay search 
      window width, the phase delay rate search window central rate, 
      the phase delay rate search window width, the oversampling factor over 
      group delay, the oversampling factor over delay rate, the fringe fringe 
      search algorithm name, the autocorrelation threshold, the visibility 
      weight threshold, the signal to noise detection limit, and the 
      threshold of the normalized noise level for the outlier rejection.
      The fringe fitting task has the ability to generate plots. The control 
      file defines the type of the fringe plot, its format, the delay window 
      width delay, the phase delay rate window width, the plotting oversampling 
      factors, the span of the delay resolution function, and the time and 
      frequency averaging factors.

### 2.6.3 Bandpass calibration

      The phase/amplitude response of the ideal data acquisition system to
      the white noise has a \Pi-shape for the amplitude as function of 
      frequency and the frequency-independent offset for phase. In practice,
      data acquisition system causes distortions with respect to the ideal
      form that have to be calibrated. SGDASS provides the tools for 
      computation of the bandpass calibration from observations of strong radio
      sources. This is done in three steps. At the first step an observations 
      with the strongest signal to noise ratio at each baseline with the 
      reference station is identified. The initial bandpass is computed by 
      inversion of time-averaged complex residual visibilities following by 
      smoothing either using smoothing B-spline or an expansion over Legendre 
      polynomials of the specified degree. This process is repeated for all 
      baselines with the reference station. At the second step this initial 
      bandpass is applied to a set of N observations with the strongest signal 
      to noise ratios, except the one that was used for computation of the 
      initial bandpass, and the time-averaged residual complex visibilities 
      are accumulated. The process is repeated for N observations of each 
      baseline with the reference station. The inverse of the accumulated 
      complex time-averaged visibilities divided by N forms the bandpass 
      correction to the initial bandpass. The sum of the initial bandpass and 
      the accumulated correction to the initial bandpass is the accumulated 
      bandpass. At the third step the accumulated bandpass is applied to M 
      observations at each baseline with the reference station, a system 
      of linear algebraic equations that related the residual amplitude and 
      phases to station-based bandpass corrections is formed. The least square
      solution of this system of equations provides the fine bandpass. Then 
      SGDASS runs the procedure of outlier detections. And observations with 
      the highest residual is identified, and if the residuals exceeds the 
      specified threshold, then such an observation is excluded, and the 
      solution is updated. The process is repeated till either no processed
      observation has a residual above the threshold, or the number of 
      remaining observations per baseline reaches the specified minimum.

      The control file defines the following parameters for the task of 
      bandpass computation: the mode; the number of observations for bandpass 
      computation in the initial, accumulation and fine modes; the frequency 
      averaging factors in the initial, accumulation, and fine modes; the 
      maximum number of observations per baseline that can be dropped during
      fine bandpass computation; the minimum signal to noise ratio in the 
      initial, accumulation and fine modes; the amplitude and phase rejection 
      factors; the names of bandpass interpolation methods; the degree of the 
      interpolation polynomial for phase and amplitude; the number of nodes 
      for smoothing; the minimum normalized amplitude; and the name of 
      the bandpass normalization method.
  
### 2.6.4 Flagging off-source observations

      Nominally, antennas are supposed to record data only when they
      point to a source. It is rather common that some data are recorded
      when antenna is slewing and not pointed to a source. SGDASS 
      provides a utility for detection of this situation for flagging such 
      data. The utility checks every observation and computes the 
      frequency-averaged residual phase and amplitude over the subset of data 
      called a kernel that is assumed to not be affected. The kernel
      interval defines two boundaries as a fraction of the scan length
      for the good data interval. The utility computes group delays
      and phase delay rates over the kernel interval, then applies
      these group delay and phase delay rate data to each visibility
      data outside the kernel interval. It first tries the interval
      before the low boundary of the kernel interval in the backward 
      order. If it finds visibilities for K consecutive epochs
      with frequency-averaged amplitude below the specify threshold,
      it flags all these K visibilities as well as preceding visibilities.
      Then it repeats these procedure for the interval beyond the 
      upper boundary. The threshold can be set either as a fraction
      of the amplitude over the kernel interval or as factor of the
      normalized amplitude root mean square computed over all 
      frequency averaged visibilities within the kernel interval

      The control file defines the following parameters for the task of 
      detection of those visibilities that have been recorded when antennas 
      were off the source: the mode of the operation, the amplitude threshold,
      the normalized residual threshold, the maximum coherency interval, 
      the minimal number of accumulation periods with low amplitude, and
      the normalized start and normalized end time of the kernel interval.

### 2.6.5 Computation of time- and frequency- averaged visibilities for
      a given radio source.

      SGDASS software allows a user to compute time and frequency
      averaged visibility data for further imaging analysis.
      SGDASS collects all visibility data and processes them 
      scan by scan. It determines the list of subarrays and for 
      each subarray it solves for a station-based group delay,
      phase delay rate, and group delay rate using baseline-based
      group delays, phase delay rates, and group delay rates among
      those observations that are not flagged. Then the visibilities
      are phase rotated for accounting for the contribution of these 
      station-based delays and rates. The visibilities are calibrated 
      for antenna gains and for system temperature. Empirical gain
      corrections are applied if they are available. The visibilities
      with the signal to noise ratios below the specified threshold
      are discarded. Then the calibrated and phase rotated 
      visibilities are averaged over time and frequency within the 
      specified number of time and frequency bins. The averaged
      visibilities are sorted and written in FITS format in a form 
      that is compatible with third part software packages AIPS and 
      DifMap.

      The control file defines the following parameters for the task of 
      generation of the time- and frequency- averaged visibilities:
      the source name, the frequency averaging factor, the time averaging 
      factor, the weight type, polarization, the make of the subarray 
      consolidation algorithm, the gain correction file name, and the 
      signal to noise ratio threshold.

### 2.6.6 Formatting results of visibility data analysis

      SGDASS provides a utility for computation of total group delays,
      phase delay rates, and group delays in a form that is suitable 
      for geodetic and astrometric analysis. It computes the scan reference
      time common for all observations of a given scan and recomputes
      delays and rates from referred to the fringe reference time that is 
      specific for a given observation to the common scan reference time. 
      It also computes the theoretical path delay for all the observations
      at the given scan reference time. Finally, it generates the output
      data file in of the specified formats.

      The control file defines the following parameters for the task of 
      generation of a database with group delays and other results of
      fringe fitting: the database output format, the name of the algorithm 
      for computation of the scan reference time, the threshold for an 
      increase of group delay uncertainty due to the offset of the scan 
      reference time with respect to the fringe reference time, the VLBI 
      catalogue configuration file name, the name of the auxiliary 
      description file, and the database suffix.


## 2.7   Processing telemetry from ground antennas


      SGDASS provides tools for processing auxiliary information captured
      by the software that control ground radio telescopes. This includes
      parsing log files in different formats, extraction of system 
      temperature, phase and amplitudes of phase calibration signals,
      records of system equivalent flu density and clock differences
      between formatter time and GPS time. It has an ability to clean
      the data for outliers, average data over time and restore missing
      data by extrapolating.


## 2.8   Scheduling VLBI observations 


      SGDASS provides a tool for scheduling VLBI observations. A VLBI
      schedule consists of two parts: radio technical and astronomical.
      The radio technical part defines commands for setting up hardware,
      such as local oscillator frequencies, intermediate frequencies,
      phase calibration frequencies, recording rate, firing amplitude 
      calibration, and similar commands. The astronomical part defines
      the sequence of slewing to a source with specified coordinates, 
      execution of a pre-observation procedure, tracking the source
      for specified time, and execution of a post-observation procedure. 
      The sequence is repeated.

      The VLBI scheduling tool is controlled by the configuration file
      that defines the experiment code; experiment description; schedule
      revision number; name of the principle investigator; the scheduling
      algorithm name; the names file file with the primary catalogue of
      target sources; the secondary catalogue of target sources; the
      catalogue of calibrating source; the experiment start time; the
      experiment stop time; the gaps in the experiment; the default scan 
      time; the interval time between bursts of troposphere calibration; 
      the algorithm for scheduling troposphere calibrating sources; the
      scan length of observation of calibrating sources; the minimum number 
      of stations in scans of calibrating source observations; the margin
      of avoidance of 180 deg azimuth at beginning of an observation;
      the minimum, maximum and normal number of scans for source;
      the minimum and normal interval time between observations of the same
      source; the maximum number of schedule sources; the planetary 
      ephemeride; the minimum distance to the Sun; the list of participating 
      stations; the name of the directory with station parameters; the name 
      of the file with a header; the name of VLBI hardware setups; the 
      observing mode name, the observing mode description, and names of 
      the output files.
     
      Scheduling software parses a control file, parses the specified files 
      with source catalogues, and computes the time ranges for each source 
      when it is visible at every observing stations. Using this information, 
      scheduling checks for the nominal experiment start time of all the 
      sources that are above the specified elevation limits and computes the 
      score in accordance with the specified algorithm. A source with the 
      highest score is selected. Time for data recording and the maximum 
      slewing time to that sources among participating stations is accounted 
      for, the epoch of a new observation is set. The process is repeated till 
      the nominal experiment date is reached. The scoring algorithm considers 
      different factors for computation of the score that prioritize 
      observation of a given sources with respect to another. These factors 
      include history of prior observations of a given source in the scheduled
      experiment and in the entire campaign; time elapsed from prior 
      observation of a given source; source declination; azimuth and elevation 
      of prior observations; slew time; the  arc length between the source
      of prior observation, and the source priority parameter. Scheduling of 
      the target source is interrupted over the specified interval time, and 
      the specified number of calibrator scans is inserted to the schedule. 
      The source observed in calibrating scans are selected in accordance with
      the specified algorithm. That algorithm accounts for observations at 
      elevations above and below some threshold.

      The sequence of scheduled sources is written in the output file together
      with other relevant information that includes hardware setup.


# 3.    Layer of numerical methods implementation


      The scientific layer of SGDASS relies on software that implements general
      procedures of numerical methods. Some these procedures implement 
      numerical methods from scratches, some of them a form of middle-ware to
      open source general purpose libraries that implement fast Fourier
      transform (FFT), basic linear algebra (BLAS), or Linear Algebra Package
      (LAPACK). Although the scientific layer is rather specific to processing
      data of certain space geodesy technique, the numerical method layer
      is more general and has its applicability beyond space geodesy.
      

## 3.1   Spherical harmonics and digital filtration routines


      SGDASS provides a highly optimized code that implements fast spherical 
      harmonics transformation, direct and inverse. It provides also routines
      for computation of the value of a function using its spherical harmonic
      transforms and partial derivatives over longitude and latitude, and
      routines for computation of the power spectrum of spherical harmonics.
      SGDASS provides routines for spherical harmonics transform evaluation
      using least squares and fast routines for a direct and inverse spherical 
      harmonic transform on a regular latitude/longitude grid invoking Driscoll 
      and Healy (1994) sampling theorem. The routines utilizes OpenMP 
      parallelization. The routines do not lose accuracy at high degree/order. 
      For dimensions less than 2700 the algorithm of Holmes and Featherstone 
      (2002) is used. For dimensions higher than 2700 the algebra of X-number 
      introduced by Fukushima (2012) is used on order to avoid numerical 
      catastrophe at the polar regions related to the insufficient range of 
      double precision numbers. Spherical harmonics transform of degree/order 
      as high as 65535 is supported.

      SGDASS provides a convenient interface to fast Fourier transform (FFT).

      SGDASS provides code implementing digital filter in frequency domain. 
      A filter of this kind involves three operations: forward FFT, 
      multiplication the spectrum by a window function, backward FFT.
      SGDASS provides routines for low-pass, high-pass, band-pass filters.


## 3.2   B-spline expansion routines


      SGDASS heavily relies on a formalism of expansion over basic splines
      (or B-splines). It provides highly optimized functions for computation
      of B-spline value as well as  first, second, and third derivatives; 
      B-spline integral, double integral, and triple integral; B-spline first 
      and second momentum; and B-spline of a sine and cosine constituents 
      of the Fourier transform  at a given frequency. SGDASS also provides 
      routines for computation of the integral or a series of integrals over 
      the specified range using the B-spline coefficients or a matrix of 
      multiple B-spline expansions. SGDASS provides procedures for B-spline 
      computation in two cases: interpolating B-spline when the number of  
      coefficients is equal to the number of knots and smoothing B-spline
      when the number of coefficients is less than the number of knots. 
      In a latter case B-spline coefficients are estimated using least squares.
      SGDASS provides routines for solving this problem for a number of 
      commonly used cases: a case of stabilizing constraints imposed on the 
      value, or/and on the first or/and on second derivatives and a case of 
      estimation of B-spline and linear trend with imposing decorrelation 
      constraints. SGDASS provides routines for computation of interpolating 
      B-spline and smoothing B-spline for an one dimensional array, 
      a two-dimensional array, a three-dimensional array, or a four-dimensional 
      array. SGDASS provides routine of B-spline computation for single and 
      double precision. It also provides routines for computation of B-spline 
      of the 3rd degree and its derivatives with additional optimizations that 
      are achieved by manual loop unrolling.
       

## 3.3   Chebyschev and Legendre polynomial polynomial expansion routines


      SGDASS relies on the apparatus of polynomial approximation. It provides 
      routines for computation of Chebyschev and Legendre polynomials,
      the first and second derivatives as well as  integrals. It provides
      routines for computation of expansion of a given function into the 
      polynomial basis using least squares with imposing constraints.


## 3.4   Linear algebra routines


      SGDASS provides routines for linear algebra that fall into two 
      categories. Routines of the first category provide a convenient
      interface for Basic Linear Algebra (BLAS) and Linear Algebra 
      Package (LAPACK) that best serve the needs of space geodesy
      data analysis. They include computation of multiplication of matrices,
      multiplication of matrix and vectors, matrix inversion, eigenvector 
      decomposition, scattering and gathering sparse matrix, Cholesky 
      decomposition, solving linear equations that correspond to non-sparse 
      matrix, solving linear equations that corresponds to the band matrix 
      and related procedures. The routines of the second category implement
      routines solving those linear algebra problems that are not present in 
      BLAS and LAPACK libraries. That includes highly efficient routine for 
      inversion of a square matrix in the upper triangular representation, 
      computation of condition numbers, recursive Cholesky decomposition, 
      recursive solution of linear equations, computation of products of 
      sparse matrix and sparse vectors in different representations, manually 
      inlined versions of matrix operations for small dimensions and related 
      procedures.


## 3.5   Linear regression routines


      SGDASS provides a number of routines for linear regression, with
      and without weights and ability to automatically remove outliers and
      for computation of the weighted root mean squares of residuals.


## 3.6   Time transformation routines


      SGDASS provides a number of routines to transformation of time 
      and date according to various conventions used in astronomy.
      That includes calendar date, Julian date, modified Julian date,
      day of year, and Unix time.


## 3.7   Geodetic transformation routines


      SGDASS provides a number of routines for geodetic transformation:
      transformation between Cartesian coordinates to spherical coordinates,
      elliptical coordinates, local topocentirc coordinates, as well as 
      inverse transformations. It also includes routines for computation of 
      the local gravity acceleration and the height of the geoid above the 
      reference ellipsoid. 
      

# 4.    I/O support level


## 4.1   Geo VLBI database handler


      SGDASS provides a custom database handler tuned for processing geodetic
      VLBI experiments. A database file contains both text information
      related to the experiment description, logs of the pipeline procedures,
      and a collection of one- and two-dimensional arrays with data. 
      A database file contains several sections. The provenance section
      keeps the names of the input files that were used to create the database,
      date of database creation, and history of updates. Text sections contain
      plain ascii information organized in chapter. The tables of contents 
      define arrays, their type, scope, dimensions, and provide their a short 
      description. The data section keeps the data as set of records of 
      different length and different type. The address of a data record is 
      computed based in the table of contents.

      The SGDASS database handler provides low level public routines that 
      implement basic operations of opening and closing the database file, 
      populating or inquiring the table of contents, and reading or writing 
      the data. The handler has also a number of internal routines for creation
      and sorting hash tables, for computation of the record address, splitting
      and concatenating buffers with text information and and similar routines.

      The SGDASS database handler supports the same database contents in two 
      forms, binary and ascii. It provides utilities for a lossless 
      transformation between binary format to ascii and both. The SGDASS 
      database handler provides also procedures for ingestion of data in vgosdb 
      format.


## 4.2   Support of automatic services for retrieving
      data from external servers that are required
      for data analysis.


      SGDASS implements procedures for the automatic retrievals of data from
      external hosts for their consecutive processing. These data are the 
      output of numerical weather model, global ionospheric maps, and the 
      Earth orientation parameters. The routines maintain the local lists 
      of original or processed data, atomically query the contents of 
      remote servers, compare the lists, generate the list of datasets
      that are present at remote servers, but are absent from the local
      host, initiate data retrieval, check the integrity of the retrieved
      data, and initiate a specified data analysis procedure for the 
      retrieved datasets. In addition, they checks the integrity of the local
      repository with processed results, and it finds failures, it 
      automatically cleans damaged datasets and repeats retrieval. This 
      process is very robust and is designed to work in situations when
      the local and/or remote server crushes and/or is rebooted.


## 4.3   Interface to cfitsio library for read/write data in FITS-format


      SGDASS provides middleware routines to cfitsio software that
      reads, writes, or updates data in FITS-format. This layer of 
      software facilitates supports of processing data in FITS-format.
      These routines include retrieval of the list of keys, getting 
      and/or putting arrays with data, and provide interface to cfitsio
      error messages.


## 4.4   Interface to Unix low level I/O routines


      SGDASS provides middleware interface to portable routines for the low
      level input/output. That includes routines of searching files in 
      a directory  tree, opening/closing files, reading writing 
      user-defined records, reading and writing big files.

      
# 5.    Graphics support level


## 5.1   Library of the Dialogue graphic interface


      DiaGI (Dialogue Graphic Interface) is a library of subroutines which 
      allows to draw a plot of one or several one-dimension functions 
      by using only one operator, then, if necessary, it interactively changes 
      boundaries of the plotting area, changes point style, line style, 
      line width, error bar style, allows to make a hard copy in PostScript or 
      GIF formats with or without sending it at the printing device, and 
      eventually it returns execution of the calling program. DiaGI makes work 
      with graphic as easy as possible and frees user from cumbersome graphic 
      programming. A user can put a short statement at any place of his or her 
      code, look at the plot, transform it, print it, and then continue execution 
      of the main program.

      DiaGI may be called in two ways: with using a simplified interface and 
      wth using a verbose interface. The usage of the verbose interface assumes 
      filling fields of the data structure declaring initial parameters of the 
      plotting and addresses of the arrays of the arguments, values and errors 
      of the functions to be plotted. This way is not recommended for a routine 
      work unless special effects are needed. The simplified interface assumes 
      specifying only the number of points and arrays of the arguments, 
      values and, optionally, errors of the function to be plotted. Initial 
      values of boundaries of the plotting area, point style, line style, line 
      width, error bar style will be set up automatically in accordance with 
      defaults which can be changed by specifying environment variables. 
      If necessary, a user can easily change plotting parameters interactively.

      DiaGI allows to re-define colors in order to adjust user preferences. 


## 5.2   Tools for 2D visualization


      SGDASS provides tools for visualization of parameters defined on the 
      Earth's surface on the latitude-longitude grid. It also provides
      tools for visualization of image data and calibrated visibilities 
      stored in FITS-IDI format.


# 6.    Interface to operating system support level


      SGDASS provides a set of routines that serve as a middleware between
      the a process and the UNIX operating system. This eliminates a known 
      deficiency of UNIX operating system that provides access to system 
      calls only from C programming language. The middleware routines 
      implement this interface to other language. This includes access to 
      timer, processing system signals, socket level network interface, 
      probing an address, process sleep, low level terminal input/output, 
      getting information about a file, matching regular expressions, 
      setting stack size, and getting system parameters.

# 7.0   Notices and disclaimer


# 7.1   Notices

      Copyright (c) 2010-2024 United States Government as represented by the 
      Administrator of the National Aeronautics and Space Administration.  
      All Rights Reserved.

# 7.2   Disclaimer

      No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY 
      WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, 
      INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE 
      WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF 
      MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM 
      INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR 
      FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM 
      TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, 
      CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT 
      OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY 
      OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  
      FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES 
      REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, 
      AND DISTRIBUTES IT "AS IS."

# 7.3   Waiver and Indemnity

      RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES 
      GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR 
      RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY 
      LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, 
      INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, 
      RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND 
      HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND 
      SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED
      BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE 
      IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
