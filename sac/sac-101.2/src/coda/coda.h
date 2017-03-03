
/* needs to be in an include file */
#define MAXDATA 1000000
#define MAXBANDS 50
#define SCRATCH_SIZE 100000
#define NPTS_GF 5000
#define START_GF 20
#define DELTA_GF 1.0
#define START_WINDOW_ADD 20
#define MAX_RESIDUAL .10
#define DIST_CRITICAL 350.0
#define GAMMA 1.0
#define MAX_MOMENT_INDEX 5
#define MW_MULTIPLIER 0.66666
#define MW_SUBTRACT 10.7
#define FIT_FUNC_MULT 0.000001
#define FIT_FUNC_SUB  100
#define FIT_INITIAL_LENGTH 200.
#define FIT_NPTS_POW 1.4
#define NOISE_START 0.0
#define NOISE_LENGTH 100.0
#define MINIMUM_SNR 0.0
#define YVPMAX 30.0
#define YVPMIN 0.0
struct envelope{
     float       freq_low;
     float       freq_high;
     float       *envelope_data;
     int         number_of_points;
     int         window_start;
     int         window_stop;
     float       window_start_seconds;
     float       coda_amplitude;
     float       *GFenvelope;
     float       GFstart;
     int         GFnpts;
     float       GFdelta;
     float       b0;
     float       vel0;
     float       vel1;
     float       vel2;
     float       vel_int;
     float       vel_slope;
     float       dist_critical;
     float       gamma;
     float       min_length;
     float       minimum_SNR;
     float       max_residual;
     float       noise_thresh;
     float       b_slope;
     float       dist_slope;
     float       dist_order1;
     float       dist_order2;
     float       Moment_correction;
     int         fit_npoints;
     int         fit_window_picked;
     float       CodaAmp;
     float       fit_residual;
     float       fit_SNR;
     float       fit_b0;
     float       fit_gamma;
     float       Moment;
     float       Mb_weight;
     char        ResidStat[2];
     char        SNRStat[2];
   };
struct global_params{
     float Mb_scale;
     float Mb_constant;
     float Moment;
     float Mw;
     float Mb;
     float Energy_Obs;
     float Energy_High;
     float Energy_Low;     
     float Energy_Total;
     float Stress_Total;
     float Max_Residual;
     float Minimum_SNR;
   };
int nerr;

