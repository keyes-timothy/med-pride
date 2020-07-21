Outness report
================
Timothy Keyes
2020-05-07

  - [Data quality and cleaning](#data-quality-and-cleaning)
  - [Univariate Demographics report](#univariate-demographics-report)
      - [What school do you attend?](#what-school-do-you-attend)
      - [All other demographic
        variables](#all-other-demographic-variables)

## Data quality and cleaning

I’ve already done some data cleaning in previous scripts, but there are
still a few tweaks that we probably want to make so that everything is
human-readable and so that only useful information is carried forward.

First, we want to remove people who did not consent to the survey, since
we can’t use their information anyway.

And then we can filter out some variables that we don’t need.

And create a few variables that we’ll need later:

## Univariate Demographics report

The first section of the survey asked questions about respondents’
demographic identifiers, including the following:

  - What school they attend (`school_attend`)
  - What year of medical school they’re in (`med_school_year`)
  - Whether or not they identify as LGBTQ+ (`is_lgbtq`)
  - What their current gender identity is (variables that start with
    `gender_`)
  - What their current sexual orientation is (variables that start with
    `so_`)
  - Their race/ethnicity (variables that start with `race_`)

Below, we calculate the breakdown of our survey respondents by each of
these demographics.

### What school do you attend?

Schools are arranged in order of decreasing number of responses.

| School                                                                                | Number of responses | Percentage of total responses |
| :------------------------------------------------------------------------------------ | ------------------: | ----------------------------: |
| Temple University School of Medicine                                                  |                  84 |                           7.1 |
| University of Louisville School of Medicine                                           |                  62 |                           5.2 |
| University of Oklahoma College of Medicine                                            |                  57 |                           4.8 |
| Geisinger Commonwealth School of Medicine                                             |                  44 |                           3.7 |
| Stanford University School of Medicine                                                |                  40 |                           3.4 |
| University of Pittsburgh School of Medicine                                           |                  39 |                           3.3 |
| University of New Mexico School of Medicine                                           |                  37 |                           3.1 |
| Saint Louis University School of Medicine                                             |                  34 |                           2.9 |
| University of Alabama School of Medicine                                              |                  34 |                           2.9 |
| University of Arkansas for Medical Sciences/UAMS College of Medicine                  |                  34 |                           2.9 |
| Johns Hopkins University School of Medicine                                           |                  32 |                           2.7 |
| University of Michigan Medical School                                                 |                  32 |                           2.7 |
| Western Michigan University Homer Stryker M.D. School of Medicine                     |                  30 |                           2.5 |
| Dell Medical School at The University of Texas at Austin                              |                  28 |                           2.4 |
| Chicago Medical School of Rosalind Franklin University of Medicine and Science        |                  26 |                           2.2 |
| Touro University California College of Osteopathic Medicine                           |                  23 |                           1.9 |
| University of Wisconsin School of Medicine and Public Health                          |                  23 |                           1.9 |
| Georgetown University School of Medicine                                              |                  22 |                           1.9 |
| Tulane University School of Medicine                                                  |                  22 |                           1.9 |
| Jacobs School of Medicine and Biomedical Sciences, University at Buffalo              |                  21 |                           1.8 |
| Rutgers New Jersey Medical School                                                     |                  18 |                           1.5 |
| University of Texas Southwestern Medical School at Dallas                             |                  17 |                           1.4 |
| Harvard Medical School                                                                |                  15 |                           1.3 |
| Columbia University Roy and Diana Vagelos College of Physicians and Surgeons          |                  14 |                           1.2 |
| Washington University School of Medicine                                              |                  14 |                           1.2 |
| Weill Cornell Medical College                                                         |                  14 |                           1.2 |
| NA                                                                                    |                  14 |                           1.2 |
| Albert Einstein College of Medicine                                                   |                  13 |                           1.1 |
| Alpert Medical School at Brown University                                             |                  13 |                           1.1 |
| University of California, Irvine School of Medicine                                   |                  13 |                           1.1 |
| University of Texas School of Medicine at San Antonio                                 |                  11 |                           0.9 |
| University of Vermont College of Medicine                                             |                  11 |                           0.9 |
| Cooper Medical School of Rowan University                                             |                  10 |                           0.8 |
| University of Iowa Roy J. and Lucille A. Carver College of Medicine                   |                   9 |                           0.8 |
| University of Kansas School of Medicine                                               |                   9 |                           0.8 |
| Vanderbilt University School of Medicine                                              |                   9 |                           0.8 |
| West Virginia University School of Medicine                                           |                   9 |                           0.8 |
| Edward Via College of Osteopathic Medicine                                            |                   8 |                           0.7 |
| Keck School of Medicine of University of Southern California                          |                   8 |                           0.7 |
| University of North Texas Health Science Center Texas College of Osteopathic Medicine |                   8 |                           0.7 |
| Baylor College of Medicine                                                            |                   7 |                           0.6 |
| International                                                                         |                   7 |                           0.6 |
| New York University School of Medicine                                                |                   7 |                           0.6 |
| Philadelphia College of Osteopathic Medicine - Georgia Campus                         |                   7 |                           0.6 |
| State University of New York Downstate Medical Center College of Medicine             |                   7 |                           0.6 |
| University of South Alabama College of Medicine                                       |                   7 |                           0.6 |
| Michigan State University College of Human Medicine                                   |                   6 |                           0.5 |
| University of California, San Fransisco School of Medicine                            |                   6 |                           0.5 |
| Yale School of Medicine                                                               |                   6 |                           0.5 |
| David Geffen School of Medicine at UCLA                                               |                   5 |                           0.4 |
| Northwestern University Feinberg School of Medicine                                   |                   5 |                           0.4 |
| University of Arizona College of Medicine - Phoenix                                   |                   5 |                           0.4 |
| University of California, Davis School of Medicine                                    |                   5 |                           0.4 |
| University of Florida College of Medicine                                             |                   5 |                           0.4 |
| Lake Erie College of Osteopathic Medicine                                             |                   4 |                           0.3 |
| Meharry Medical College School of Medicine                                            |                   4 |                           0.3 |
| Pennsylvania State University College of Medicine                                     |                   4 |                           0.3 |
| Perelman School of Medicine at the University of Pennsylvania                         |                   4 |                           0.3 |
| Philadelphia College of Osteopathic Medicine                                          |                   4 |                           0.3 |
| Touro College of Osteopathic Medicine                                                 |                   4 |                           0.3 |
| University of Utah School of Medicine                                                 |                   4 |                           0.3 |
| University of Washington School of Medicine                                           |                   4 |                           0.3 |
| Des Moines University College of Osteopathic Medicine                                 |                   3 |                           0.3 |
| Indiana University School of Medicine                                                 |                   3 |                           0.3 |
| Medical College of Wisconsin                                                          |                   3 |                           0.3 |
| Oklahoma State University Center for Health Sciences College of Osteopathic Medicine  |                   3 |                           0.3 |
| Sidney Kimmel Medical College at Thomas Jefferson University                          |                   3 |                           0.3 |
| Stony Brook University School of Medicine                                             |                   3 |                           0.3 |
| Tufts University School of Medicine                                                   |                   3 |                           0.3 |
| University of Chicago Pritzker School of Medicine                                     |                   3 |                           0.3 |
| University of Texas Medical School at Houston                                         |                   3 |                           0.3 |
| A. T. Still University Kirksville College of Osteopathic Medicine                     |                   2 |                           0.2 |
| Dartmouth College Geisel School of Medicine                                           |                   2 |                           0.2 |
| Edward Via College of Osteopathic Medicine- Carolinas Campus                          |                   2 |                           0.2 |
| Emory University School of Medicine                                                   |                   2 |                           0.2 |
| Hackensack Meridian School of Medicine                                                |                   2 |                           0.2 |
| Icahn School of Medicine at Mount Sinai                                               |                   2 |                           0.2 |
| Loma Linda University School of Medicine                                              |                   2 |                           0.2 |
| Ohio University Heritage College of Osteopathic Medicine                              |                   2 |                           0.2 |
| Oregon Health & Science University School of Medicine                                 |                   2 |                           0.2 |
| Rowan University School of Osteopathic Medicine                                       |                   2 |                           0.2 |
| Rutgers Robert Wood Johnson Medical School                                            |                   2 |                           0.2 |
| Sanford School of Medicine of the University of South Dakota                          |                   2 |                           0.2 |
| Texas Tech University Health Sciences Center School of Medicine                       |                   2 |                           0.2 |
| Touro University Nevada College of Osteopathic Medicine                               |                   2 |                           0.2 |
| University of Illinois at Urbana-Champaign Carle Illinois College of Medicine         |                   2 |                           0.2 |
| University of Illinois College of Medicine                                            |                   2 |                           0.2 |
| University of Massachusetts Medical School                                            |                   2 |                           0.2 |
| University of Nebraska College of Medicine                                            |                   2 |                           0.2 |
| University of Rochester School of Medicine and Dentistry                              |                   2 |                           0.2 |
| University of South Carolina School of Medicine                                       |                   2 |                           0.2 |
| University of Toledo College of Medicine                                              |                   2 |                           0.2 |
| Wayne State University School of Medicine                                             |                   2 |                           0.2 |
| Boonshoft School of Medicine at Wright State University                               |                   1 |                           0.1 |
| Boston University School of Medicine                                                  |                   1 |                           0.1 |
| Burrell College of Osteopathic Medicine at New Mexico State University                |                   1 |                           0.1 |
| Campbell University School of Osteopathic Medicine                                    |                   1 |                           0.1 |
| Central Michigan University College of Medicine                                       |                   1 |                           0.1 |
| Charles R. Drew University of Medicine and Science                                    |                   1 |                           0.1 |
| Creighton University School of Medicine                                               |                   1 |                           0.1 |
| Donald and Barbara Zucker School of Medicine at Hofstra/Northwell                     |                   1 |                           0.1 |
| Drexel University College of Medicine                                                 |                   1 |                           0.1 |
| Duke University School of Medicine                                                    |                   1 |                           0.1 |
| Eastern Virginia Medical School                                                       |                   1 |                           0.1 |
| Florida Atlantic University Charles E. Schmidt College of Medicine                    |                   1 |                           0.1 |
| Florida State University College of Medicine                                          |                   1 |                           0.1 |
| Frank H. Netter M.D. School of Medicine at Quinnipiac University                      |                   1 |                           0.1 |
| Marian University College of Osteopathic Medicine                                     |                   1 |                           0.1 |
| Mayo Clinic College of Medicine                                                       |                   1 |                           0.1 |
| Michigan State University College of Osteopathic Medicine                             |                   1 |                           0.1 |
| Nova Southeastern University College of Osteopathic Medicine                          |                   1 |                           0.1 |
| Oakland University William Beaumont School of Medicine                                |                   1 |                           0.1 |
| Ross University School of Medicine & Veterinary Medicine                              |                   1 |                           0.1 |
| Rush Medical College                                                                  |                   1 |                           0.1 |
| San Juan Bautista School of Medicine                                                  |                   1 |                           0.1 |
| Southern Illinois University School of Medicine                                       |                   1 |                           0.1 |
| State University of New York Upstate Medical University                               |                   1 |                           0.1 |
| The Ohio State University College of Medicine                                         |                   1 |                           0.1 |
| University of Colorado School of Medicine                                             |                   1 |                           0.1 |
| University of Kentucky College of Medicine                                            |                   1 |                           0.1 |
| University of Maryland School of Medicine                                             |                   1 |                           0.1 |
| University of Minnesota Medical School                                                |                   1 |                           0.1 |
| University of Nevada, Las Vegas School of Medicine                                    |                   1 |                           0.1 |
| University of North Carolina School of Medicine                                       |                   1 |                           0.1 |
| University of Texas Medical Branch School of Medicine                                 |                   1 |                           0.1 |
| University of Virginia School of Medicine                                             |                   1 |                           0.1 |
| Washington State University Elson S. Floyd College of Medicine                        |                   1 |                           0.1 |
| West Virginia School of Osteopathic Medicine                                          |                   1 |                           0.1 |

### All other demographic variables

    ## [1] "<table class=\"Rtable1\">\n<thead>\n<tr>\n<th class='rowlabel firstrow lastrow'></th>\n<th class='firstrow lastrow'><span class='stratlabel'>1<br><span class='stratn'>(N=57)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>2<br><span class='stratn'>(N=134)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>3<br><span class='stratn'>(N=14)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=205)</span></span></th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>factor(sex)</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>0</td>\n<td>28 (49.1%)</td>\n<td>91 (67.9%)</td>\n<td>7 (50.0%)</td>\n<td>126 (61.5%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>1</td>\n<td class='lastrow'>29 (50.9%)</td>\n<td class='lastrow'>43 (32.1%)</td>\n<td class='lastrow'>7 (50.0%)</td>\n<td class='lastrow'>79 (38.5%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>age</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>55.1 (17.9)</td>\n<td>50.0 (15.9)</td>\n<td>65.3 (10.9)</td>\n<td>52.5 (16.7)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>56.0 [14.0, 95.0]</td>\n<td class='lastrow'>52.0 [4.00, 84.0]</td>\n<td class='lastrow'>65.0 [49.0, 86.0]</td>\n<td class='lastrow'>54.0 [4.00, 95.0]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>factor(ulcer)</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>0</td>\n<td>16 (28.1%)</td>\n<td>92 (68.7%)</td>\n<td>7 (50.0%)</td>\n<td>115 (56.1%)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>1</td>\n<td class='lastrow'>41 (71.9%)</td>\n<td class='lastrow'>42 (31.3%)</td>\n<td class='lastrow'>7 (50.0%)</td>\n<td class='lastrow'>90 (43.9%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>thickness</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>4.31 (3.57)</td>\n<td>2.24 (2.33)</td>\n<td>3.72 (3.63)</td>\n<td>2.92 (2.96)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>3.54 [0.320, 17.4]</td>\n<td class='lastrow'>1.36 [0.100, 12.9]</td>\n<td class='lastrow'>2.26 [0.160, 12.6]</td>\n<td class='lastrow'>1.94 [0.100, 17.4]</td>\n</tr>\n</tbody>\n</table>\n"
