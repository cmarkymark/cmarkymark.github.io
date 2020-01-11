import React from 'react';
import { Link } from 'react-router-dom';
import './Dropdown.css';

class Dropdown extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      dropdown: false
    }
  }

  mouseEnter = () => {
		this.setState({
			dropdown: true
		});
	}

	mouseLeave = () => {
		this.setState({
			dropdown: false
		});
	}

  render() {
    const dropdown = this.state.dropdown;
    let path;
    if (this.props.heading === "Home") {
      path = "/";
    } else {
      path = "/" + this.props.heading;
    }
    let links = [];
    if (this.props.drop) {
      for (let i = 0; i<this.props.drop.length; i++) {
        let path = "/" + this.props.drop[i].title.replace(/\s/g , "-");
        links.push(
          <Link to={path} key={this.props.drop[i].title} className="navLinks">
            <h5>{this.props.drop[i].title}</h5>
          </Link>
        );
      }
    }
    return (
      <div className="navItem" onMouseEnter={this.mouseEnter} onMouseLeave={this.mouseLeave}>
        <Link to={path}>
          <h3>{this.props.heading}</h3>
        </Link>
        {dropdown && (
          <div className="dropdown">
            <div className="dropContainer">
              {links}
            </div>
          </div>
        )}
      </div>
    );
  }

}

export default Dropdown;
