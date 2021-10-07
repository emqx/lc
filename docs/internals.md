

# flagman


## Overall

**flagman** is a long-living, supervised process which only monitors the system [runq](#org0640bc8) and ensures [actions](#org66f20a0) are taken when conditions are met.

By design, the process runs as high priority and avoids communicating with other processes.


## Terminology

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Term</th>
<th scope="col" class="org-left">Definition</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Runq<a id="org0640bc8"></a></td>
<td class="org-left">The length of beam system run_queue, checked periodly, see <a href="#orgf681128">T1</a> and <a href="#org117e8d7">T2</a>.</td>
</tr>


<tr>
<td class="org-left">Scheduler<a id="orgaf044a0"></a></td>
<td class="org-left">Number of <b>Online</b> schedulers.</td>
</tr>


<tr>
<td class="org-left">Credits<a id="org92d52d2"></a></td>
<td class="org-left">Gain credits when <a href="#org0640bc8">runq</a> is under limit, lose credits when <a href="#org0640bc8">runq</a> is over limit.</td>
</tr>


<tr>
<td class="org-left">Priority Group<a id="org402a1b3"></a></td>
<td class="org-left">Process could join/leave prioriy groups. Load contol takes different actions when conditions are meet.</td>
</tr>


<tr>
<td class="org-left">Overloaded<a id="org7c22e1e"></a></td>
<td class="org-left">State when <a href="#org92d52d2">credits</a> runs out. System is overloaded and action must be taken.</td>
</tr>


<tr>
<td class="org-left">Warm<a id="orgfc2dce4"></a></td>
<td class="org-left">State when system is losing/gaining <a href="#org92d52d2">credits</a>, a middle state preventing system is shifting between <a href="#org9afb141">cold</a> and <a href="#org7c22e1e">overloaded</a></td>
</tr>


<tr>
<td class="org-left">Cold<a id="org9afb141"></a></td>
<td class="org-left">State when system is full of <a href="#org92d52d2">credits</a></td>
</tr>


<tr>
<td class="org-left">Flag<a id="org1bb8b96"></a></td>
<td class="org-left">Regisered process name when system is <a href="#org7c22e1e">overloaded</a></td>
</tr>


<tr>
<td class="org-left">Actions<a id="org66f20a0"></a></td>
<td class="org-left">Actions to take to cool down the system. Action is taken by either flagman or API caller</td>
</tr>
</tbody>
</table>


## [Actions](#org66f20a0) to take:


### Control the [flag](#org1bb8b96)

-   ensure the [flag](#org1bb8b96) is rasied when system is <a id="org5cd7124"></a>.
-   ensure the [flag](#org1bb8b96) is cleared when system is cold


### Brutal kill processes in low (<= [F3](#org65720a8)) [priority group](#org402a1b3) in either of any conditions listed below

1.  [flag](#org1bb8b96) is rasied
2.  [credits](#org92d52d2) left percentage blow [F4](#orgb084247)


### Caller get scheduled out for a limited amount of time if the system is [overloaded](#org7c22e1e)

see `load_ctl:maydelay`

Caller is back on scheduler when [flag](#org1bb8b96) is cleared.

**TODO** Caller could be scheduled out when the system is **warm** as well?


## config parameters

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">parm name</th>
<th scope="col" class="org-left">type</th>
<th scope="col" class="org-right">default</th>
<th scope="col" class="org-left">comments</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">F0<a id="org0203e2a"></a></td>
<td class="org-left">boolean()</td>
<td class="org-right">true</td>
<td class="org-left">Set it to false to disable the load control function. To reenable, you should call <code>load_ctl:restart_runq_flagman/0</code>.</td>
</tr>


<tr>
<td class="org-left">F1<a id="org7229d2d"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">8</td>
<td class="org-left"><a href="#orgaf044a0">scheduler</a> multipler, <a href="#org0640bc8">runq</a> is overlimit when <a href="#org0640bc8">runq</a> &gt; <a href="#orgaf044a0">scheduler</a> * <a href="#org7229d2d">F1</a></td>
</tr>


<tr>
<td class="org-left">F2<a id="org0da878c"></a></td>
<td class="org-left">float()</td>
<td class="org-right">0.8</td>
<td class="org-left"><a href="#orgaf044a0">scheduler</a> utilization limit for increasing <a href="#org92d52d2">credits</a></td>
</tr>


<tr>
<td class="org-left">F3<a id="org65720a8"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">2</td>
<td class="org-left">Priority threshold for killing processes in <a href="#org402a1b3">priority group</a> forcefully.</td>
</tr>


<tr>
<td class="org-left">F4<a id="orgb084247"></a></td>
<td class="org-left">integer() &lt; 100</td>
<td class="org-right">50</td>
<td class="org-left"><a href="#org92d52d2">credits</a> left precentage.</td>
</tr>


<tr>
<td class="org-left">F5<a id="org52f839f"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">0</td>
<td class="org-left">turnaround time (in ms) threshold for extra <a href="#org92d52d2">credits</a> loss. 0 is off</td>
</tr>


<tr>
<td class="org-left">T1<a id="orgf681128"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">3000 (ms)</td>
<td class="org-left">Regular <a href="#org0640bc8">runq</a> check interval, see also <a href="#org117e8d7">T2</a></td>
</tr>


<tr>
<td class="org-left">T2<a id="org117e8d7"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">1000 (ms)</td>
<td class="org-left"><a href="#org0640bc8">runq</a> check interval when it is overlimit. See <a href="#org7229d2d">F1</a></td>
</tr>


<tr>
<td class="org-left">C2<a id="org790a2c9"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">3</td>
<td class="org-left">Cooldown <a href="#org92d52d2">credits</a>.</td>
</tr>
</tbody>
</table>


## state machine

![img](flagman_fsm.png)


# KILL [Dropped Idea] runq monitor

The main reason I drop this idea is that there seems no any benefit to make it a gen\_statm
because it should not handle any calls from other processes to minimize any undesired workload and let it be standalone.


## API


### active check system is overloaded

fun is\_overload()-> boolean()


### passive check


## state machine

![img](state_machine.png)

