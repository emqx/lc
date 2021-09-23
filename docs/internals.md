
# Table of Contents

1.  [flagman](#orgdfb1a14)
    1.  [Overall](#org67e4337)
    2.  [Terminology](#org5bd1914)
    3.  [Actions to take:](#org595a877)
        1.  [Control the flag](#org74c275d)
        2.  [Brutal kill processes in low (<= F3) priority group in either of any conditions listed below](#org40add11)
        3.  [Caller get scheduled out for a limited amount of time if the system is overloaded](#orgf659e54)
    4.  [config parameters](#orge377657)
    5.  [state machine](#org88ab2e3)
2.  [[Dropped Idea] runq monitor](#orgce945f2)
    1.  [API](#org6dc839d)
        1.  [active check system is overloaded](#orgc4fa8fd)
        2.  [passive check](#orgc4c1d17)
    2.  [state machine](#orgde9a3c8)



<a id="orgdfb1a14"></a>

# flagman


<a id="org67e4337"></a>

## Overall

**flagman** is a long-living, supervised process which only monitors the system [runq](#orgdfd8a30) and ensures [actions](#org1835078) are taken when conditions are met.

By design, the process runs as high priority and avoids communicating with other processes.


<a id="org5bd1914"></a>

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
<td class="org-left">Runq<a id="orgdfd8a30"></a></td>
<td class="org-left">The length of beam system run<sub>queue</sub>, checked periodly, see <a href="#orgbc948bd">T1</a> and <a href="#orgfd7f86d">T2</a>.</td>
</tr>


<tr>
<td class="org-left">Scheduler<a id="orgad36648"></a></td>
<td class="org-left">Number of <b>Online</b> schedulers.</td>
</tr>


<tr>
<td class="org-left">Credits<a id="org9633473"></a></td>
<td class="org-left">Gain credits when <a href="#orgdfd8a30">runq</a> is under limit, loose credits when <a href="#orgdfd8a30">runq</a> is over limit.</td>
</tr>


<tr>
<td class="org-left">Priority Group<a id="orgef57f3d"></a></td>
<td class="org-left">Process could join/leave prioriy groups. Load contol takes different actions when conditions are meet.</td>
</tr>


<tr>
<td class="org-left">Overloaded<a id="orgb8bc39d"></a></td>
<td class="org-left">State when <a href="#org9633473">credits</a> runs out. System is overloaded and action must be taken.</td>
</tr>


<tr>
<td class="org-left">Warm<a id="org5a7364c"></a></td>
<td class="org-left">State when system is loosing/gaining <a href="#org9633473">credits</a>, a middle state preventing system is shifting between <a href="#org4e89ed9">cold</a> and <a href="#orgb8bc39d">overloaded</a></td>
</tr>


<tr>
<td class="org-left">Cold<a id="org4e89ed9"></a></td>
<td class="org-left">State when system is full of <a href="#org9633473">credits</a></td>
</tr>


<tr>
<td class="org-left">Flag<a id="org812405c"></a></td>
<td class="org-left">Regisered process name when system is <a href="#orgb8bc39d">overloaded</a></td>
</tr>


<tr>
<td class="org-left">Actions<a id="org1835078"></a></td>
<td class="org-left">Actions to take to cool down the system. Action is taken by either flagman or API caller</td>
</tr>
</tbody>
</table>


<a id="org595a877"></a>

## [Actions](#org1835078) to take:


<a id="org74c275d"></a>

### Control the [flag](#org812405c)

-   ensure the [flag](#org812405c) is rasied when system is <a id="orge5ae005"></a>.
-   ensure the [flag](#org812405c) is cleared when system is cold


<a id="org40add11"></a>

### Brutal kill processes in low (<= [F3](#orgedbc380)) [priority group](#orgef57f3d) in either of any conditions listed below

1.  [flag](#org812405c) is rasied
2.  [credits](#org9633473) left percentage blow [F4](#orgd263cd2)


<a id="orgf659e54"></a>

### Caller get scheduled out for a limited amount of time if the system is [overloaded](#orgb8bc39d)

see `load_ctl:maydelay`

Caller is back on scheduler when [flag](#org812405c) is cleared.

**TODO** Caller could be scheduled out when the system is **warm** as well?


<a id="orge377657"></a>

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
<td class="org-left">F0<a id="orge6c5563"></a></td>
<td class="org-left">boolean()</td>
<td class="org-right">true</td>
<td class="org-left">Set it to false to disable the load control function. To reenable, you should call <code>load_ctl:restart_runq_flagman/0</code>.</td>
</tr>


<tr>
<td class="org-left">F1<a id="orgbb4802b"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">8</td>
<td class="org-left"><a href="#orgad36648">scheduler</a> multipler, <a href="#orgdfd8a30">runq</a> is overlimit when <a href="#orgdfd8a30">runq</a> &gt; <a href="#orgad36648">scheduler</a> * <a href="#orgbb4802b">F1</a></td>
</tr>


<tr>
<td class="org-left">F2<a id="org946ee10"></a></td>
<td class="org-left">float()</td>
<td class="org-right">0.8</td>
<td class="org-left"><a href="#orgad36648">scheduler</a> utilization limit for increasing <a href="#org9633473">credits</a></td>
</tr>


<tr>
<td class="org-left">F3<a id="orgedbc380"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">2</td>
<td class="org-left">Priority threshold for killing processes in <a href="#orgef57f3d">priority group</a> forcefully.</td>
</tr>


<tr>
<td class="org-left">F4<a id="orgd263cd2"></a></td>
<td class="org-left">integer() &lt; 100</td>
<td class="org-right">50</td>
<td class="org-left"><a href="#org9633473">credits</a> left precentage.</td>
</tr>


<tr>
<td class="org-left">T1<a id="orgbc948bd"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">3000 (ms)</td>
<td class="org-left">Regular <a href="#orgdfd8a30">runq</a> check interval, see also <a href="#orgfd7f86d">T2</a></td>
</tr>


<tr>
<td class="org-left">T2<a id="orgfd7f86d"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">1000 (ms)</td>
<td class="org-left"><a href="#orgdfd8a30">runq</a> check interval when it is overlimit. See <a href="#orgbb4802b">F1</a></td>
</tr>


<tr>
<td class="org-left">C2<a id="org300e7c8"></a></td>
<td class="org-left">integer()</td>
<td class="org-right">3</td>
<td class="org-left">Cooldown <a href="#org9633473">credits</a>.</td>
</tr>
</tbody>
</table>


<a id="org88ab2e3"></a>

## state machine

![img](flagman_fsm.png)


<a id="orgce945f2"></a>

# KILL [Dropped Idea] runq monitor

The main reason I drop this idea is that there seems no any benefit to make it a gen<sub>statm</sub>
because it should not handle any calls from other processes to minimize any undesired workload and let it be standalone.


<a id="org6dc839d"></a>

## API


<a id="orgc4fa8fd"></a>

### active check system is overloaded

fun is<sub>overload</sub>()-> boolean()


<a id="orgc4c1d17"></a>

### passive check


<a id="orgde9a3c8"></a>

## state machine

![img](state_machine.png)

