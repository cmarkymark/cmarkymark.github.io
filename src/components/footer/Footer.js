import React from 'react';
import './Footer.css';

const Footer = ({footer}) => {
  return (
    <footer>
      <div>
        <p>Charles Marks</p>
        <p>Some footer material</p>
        <p>A React.js skeleton created by <a
          href="https://github.com/philipLutz"
          target="_blank"
          rel="noopener noreferrer">
          Philip Lutz
          </a>
        </p>
      </div>
    </footer>
  );
};

export default Footer;
