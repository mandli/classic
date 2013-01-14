#!/usr/bin/env python

r"""Data file representation of classic clawpack parameters"""

import numpy as np

import clawpack.clawutil.clawdata as clawdata

class ClawInputData(clawdata.ClawData):
    r"""
    Object containing basic Clawpack input data, usually written to 'claw.data'.


    """

    def __init__(self, num_dim):
        super(ClawInputData,self).__init__()

        # Set default values:
        self.add_attribute('num_dim',num_dim)
        self.add_attribute('num_eqn',1)
        self.add_attribute('num_waves',1)
        self.add_attribute('num_aux',0)
        self.add_attribute('output_style',1)
        self.add_attribute('output_times',[])
        self.add_attribute('num_output_times',None)
        self.add_attribute('output_t0',True)
        self.add_attribute('output_step_interval',None)
        self.add_attribute('total_steps',None)
        self.add_attribute('tfinal',None)
        self.add_attribute('output_format',1)
        self.add_attribute('output_q_components','all')
        self.add_attribute('output_aux_components',[])
        self.add_attribute('output_aux_onlyonce',True)
        
        self.add_attribute('dt_initial',1.e-5)
        self.add_attribute('dt_max',1.e99)
        self.add_attribute('dt_variable',1)
        self.add_attribute('cfl_desired',0.9)
        self.add_attribute('cfl_max',1.0)
        self.add_attribute('steps_max',50000)
        self.add_attribute('order',2)
        self.add_attribute('transverse_waves',2)
        self.add_attribute('dimensional_split',0)
        self.add_attribute('verbosity',0)
        self.add_attribute('verbosity_regrid',0)
        self.add_attribute('source_split',0)
        self.add_attribute('capa_index',0)
        self.add_attribute('limiter',[4])
        self.add_attribute('t0',0.)
        self.add_attribute('num_ghost',2)
        self.add_attribute('fwave',False)
        self.add_attribute('restart',False)
        self.add_attribute('restart_file','')

        if num_dim == 1:
            self.add_attribute('lower',[0.])
            self.add_attribute('upper',[1.])
            self.add_attribute('num_cells',[100])
            self.add_attribute('bc_lower',[0])
            self.add_attribute('bc_upper',[0])
        elif num_dim == 2:
            self.add_attribute('lower',[0.,0.])
            self.add_attribute('upper',[1.,1.])
            self.add_attribute('num_cells',[100,100])
            self.add_attribute('bc_lower',[0,0])
            self.add_attribute('bc_upper',[0,0])
        elif num_dim == 3:
            self.add_attribute('lower',[0.,0.,0.])
            self.add_attribute('upper',[1.,1.,1.])
            self.add_attribute('num_cells',[100,100,100])
            self.add_attribute('bc_lower',[0,0,0])
            self.add_attribute('bc_upper',[0,0,0])
        else:
            raise ValueError("Only num_dim=1, 2, or 3 supported ")


    def write(self, out_file='claw.data', data_source='setrun.py'):
        r""""""
        self.open_data_file(out_file,data_source)

        self.data_write('num_dim')
        self.data_write('lower')
        self.data_write('upper')
        self.data_write('num_cells')
        self.data_write()  # writes blank line
        self.data_write('num_eqn')
        self.data_write('num_waves')
        self.data_write('num_aux')
        self.data_write()  # writes blank line

        self.data_write('t0')
        self.data_write()
        self.data_write('output_style')

        if self.output_style == 1:
            self.data_write('num_output_times')
            self.data_write('tfinal')
            self.data_write('output_t0')
        elif self.output_style == 2:
            if len(self.output_times) == 0:
                raise AttributeError("*** output_style==2 requires nonempty list" \
                        + " of output times")
            self.num_output_times = len(self.output_times)
            self.data_write('num_output_times')
            self.data_write('output_times')
        elif self.output_style==3:
            self.data_write('output_step_interval')
            self.data_write('total_steps')
            self.data_write('output_t0')
        else:
            raise AttributeError("*** Unrecognized output_style: %s"\
                  % self.output_style)
            

        self.data_write()
        if self.output_format in [1,'ascii']:
            self.output_format = 1
        elif self.output_format in [2,'netcdf']:
            self.output_format = 2
        else:
            raise ValueError("*** Error in data parameter: " +
                  "output_format unrecognized: ",clawdata.output_format)
            
        self.data_write('output_format')

        if self.output_q_components == 'all':
            iout_q = self.num_eqn * [1]
        elif self.output_q_components == 'none':
            iout_q = self.num_eqn * [0]
        else:
            iout_q = np.where(self.output_q_components, 1, 0)

        # Write out local value of iout_q rather than a data member
        self.data_write('', value=iout_q, alt_name='iout_q')

        if self.num_aux > 0:
            if self.output_aux_components == 'all':
                iout_aux = self.num_aux * [1]
            elif self.output_aux_components == 'none':
                iout_aux = self.num_aux * [0]
            else:
                iout_aux = np.where(self.output_aux_components, 1, 0)
            self.data_write(name='', value=iout_aux, alt_name='iout_aux')
            self.data_write('output_aux_onlyonce')

        self.data_write()
        self.data_write('dt_initial')
        self.data_write('dt_max')
        self.data_write('cfl_max')
        self.data_write('cfl_desired')
        self.data_write('steps_max')
        self.data_write()
        self.data_write('dt_variable')
        self.data_write('order')

        if self.num_dim == 1:
            pass
        else:
            if self.transverse_waves in [0,'none']:  
                self.transverse_waves = 0
            elif self.transverse_waves in [1,'increment']:  
                self.transverse_waves = 1
            elif self.transverse_waves in [2,'all']:  
                self.transverse_waves = 2
            else:
                raise AttributeError("Unrecognized transverse_waves: %s" \
                      % self.transverse_waves)
            self.data_write(file, self.transverse_waves, 'transverse_waves')

            if self.dimensional_split in [0,'unsplit']:  
                self.dimensional_split = 0
            elif self.dimensional_split in [1,'godunov']:  
                self.dimensional_split = 1
            elif self.dimensional_split in [2,'strang']:  
                self.dimensional_split = 2
            else:
                raise AttributeError("Unrecognized dimensional_split: %s" \
                      % self.dimensional_split)
            self.data_write('dimensional_split')
            
        self.data_write('verbosity')

        if self.source_split in [0,'none']:  
            self.source_split = 0
        elif self.source_split in [1,'godunov']:  
            self.source_split = 1
        elif self.source_split in [2,'strang']:  
            self.source_split = 2
        else:
            raise AttributeError("Unrecognized source_split: %s" \
                  % self.source_split)
        self.data_write('source_split')

        self.data_write('capa_index')
        if self.num_aux > 0:
            self.data_write(file, self.aux_type, 'aux_type')
        self.data_write('fwave')
        self.data_write()

        for i in range(len(self.limiter)):
            if self.limiter[i] in [0,'none']:        self.limiter[i] = 0
            elif self.limiter[i] in [1,'minmod']:    self.limiter[i] = 1
            elif self.limiter[i] in [2,'superbee']:  self.limiter[i] = 2
            elif self.limiter[i] in [3,'mc']:        self.limiter[i] = 3
            elif self.limiter[i] in [4,'vanleer']:   self.limiter[i] = 4
            else:
                raise AttributeError("Unrecognized limiter: %s" \
                      % self.limiter[i])
        self.data_write('limiter')

        self.data_write()

        self.data_write('num_ghost')
        for i in range(self.num_dim):
            if self.bc_lower[i] in [0,'user']:       self.bc_lower[i] = 0
            elif self.bc_lower[i] in [1,'extrap']:   self.bc_lower[i] = 1
            elif self.bc_lower[i] in [2,'periodic']: self.bc_lower[i] = 2
            elif self.bc_lower[i] in [3,'wall']:     self.bc_lower[i] = 3
            else:
                raise AttributeError("Unrecognized bc_lower: %s" \
                      % self.bc_lower[i])
        self.data_write('bc_lower')

        for i in range(self.num_dim):
            if self.bc_upper[i] in [0,'user']:       self.bc_upper[i] = 0
            elif self.bc_upper[i] in [1,'extrap']:   self.bc_upper[i] = 1
            elif self.bc_upper[i] in [2,'periodic']: self.bc_upper[i] = 2
            elif self.bc_upper[i] in [3,'wall']:     self.bc_upper[i] = 3
            else:
                raise AttributeError("Unrecognized bc_upper: %s" \
                      % self.bc_upper[i])
        self.data_write('bc_upper')

        self.data_write()
        self.data_write('restart')
        self.data_write('restart_file')
        self.data_write('checkpt_style')
        if self.checkpt_style==2:
            num_checkpt_times = len(self.checkpt_times)
            self.data_write(name='', value=num_checkpt_times, alt_name='num_checkpt_times')
            self.data_write('checkpt_times')
        elif self.checkpt_style==3:
            self.data_write('checkpt_interval')
        elif self.checkpt_style not in [0,1]:
            raise AttributeError("*** Unrecognized checkpt_style: %s"\
                  % self.checkpt_style)

        # self.data_write()
        self.close_data_file()