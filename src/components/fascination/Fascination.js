import React from 'react';
import { Link } from 'react-router-dom';
import './Fascination.css';

const Fascination = (props) => {
  let links = [];
  if (!props.filePaths) {
    let entries = Object.entries(props);
    for (let i = 0; i < entries.length; i++) {
      let path = "/" + entries[i][1].title.replace(/\s/g , "-");
      links.push(
        <Link to={path} key={entries[i][1].title}>
          <p>{entries[i][1].title}</p>
        </Link>
      );
    }
  }
  return (
    <div id="fascination">
      Fascination
      <div>
        {links}
      </div>
    </div>
  );
};

export default Fascination;
