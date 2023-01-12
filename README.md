[![GitHub license](https://badgen.net/github/license/Naereen/Strapdown.js)](https://github.com/ciantudur/wfamod-ltt/blob/main/LICENSE)
[![Open issues](https://img.shields.io/github/issues/ciantudur/wfamod-ltt)](https://github.com/ciantudur/wfamod-ltt/issues)
[![Version](https://img.shields.io/github/v/release/ciantudur/wfamod-ltt?display_name=tag&include_prereleases)](https://github.com/ciantudur/wfamod-ltt/releases)

<a name="top"></a>
<br />
<p align="center">
<img alt="WFA logo" width="150px" align ="center" src="https://github.com/ciantudur/wfamod-ltt/blob/main/img/wfalogo.png?raw=true" />

<h3 align="center">WFAMOD-LTT</h3>
  <p align="center">
    A microsimulation model to forecast Land Transaction Tax revenue in Wales.
  </p>
  <p align="center">
    <a href="https://github.com/ciantudur/wfamod-ltt/releases">Download the model</a>
  </p>
</div>

<br>

<a name="top"></a> 
<details closed>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
      </ul>
    </li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>


## About The Project

[Land Transaction Tax](https://gov.wales/land-transaction-tax-guide) (LTT) replaced Stamp Duty Land Tax in Wales when new powers were devolved to the Welsh Government in April 2018. The tax is payable by buyers of residential and non-residential properties.

This model allows users to forecast LTT revenues in future years by specifying parameters for house price growth and transaction volume. Since the model simulates the property price distribution, users can customise the tax rates and thresholds to gauge the cost of various policies.

The model is developed and maintained by Wales Fiscal Analysis – a research unit within the Wales Governance Centre at Cardiff University. Further information about the project and its research output can be found on the [Wales Governance Centre](https://www.cardiff.ac.uk/wales-governance-centre/publications/finance) website.



### Built With

[![R][R.js]][R-url]


## Getting Started

The model is currently designed to be run locally. Prospective users should download the repository and follow the instructions provided in the [Wiki](https://github.com/ciantudur/wfamod-ltt/wiki) to run the model.

### Prerequisites

* R
* RStudio (optional but recommended)

## Roadmap

In addition to updating the forecast parameters using the Office for Budget Responsibility's house price and transactions forecast, we hope to continue developing the model by adding new features and improving user accessibility.


- [X] First public release (v1.0.0-beta) **(October 2022)**
- [ ] Update parameters using latest OBR forecast **(Late 2022)**
- [ ] Launch online web interface **(TBC)**

See the [open issues](https://github.com/ciantudur/wfamod-ltt/issues) for a full list of proposed features (and known issues).

If you have suggestions that would improve the model, please fork the repo and create a pull request. Alternatively, you can open an issue with the tag "enhancement" or get in touch.

## License

Distributed under the MIT License. See `LICENSE.txt` for more information.


## Contact

Cian Sion - [@ciantudur](https://twitter.com/ciantudur) - sionc1@cardiff.ac.uk

Project Link: [https://github.com/ciantudur/wfamod-ltt](https://github.com/ciantudur/wfamod-ltt)


## Acknowledgments
This model has been made possible by the following open-source projects:

* [R Project]()



<!-- MARKDOWN LINKS & IMAGES -->
[R.js]: https://img.shields.io/badge/r-3864BA?style=for-the-badge&logo=r&logoColor=white
[R-url]: https://www.r-project.org/

[wfa-url]: https://www.cardiff.ac.uk/wales-governance-centre/publications/finance
